package chanterelle.internal

import chanterelle.internal.Sources.Ref
import chanterelle.internal.Structure.*
import chanterelle.internal.Transformation.Field

import scala.collection.Factory
import scala.collection.immutable.VectorMap
import scala.quoted.*

import NamedTuple.*
import chanterelle.Mode
import chanterelle.internal.Debug.AST

private[chanterelle] object Interpreter {

  def runTransformation(primary: Expr[Any], transformation: Transformation[Nothing])(using
    Sources,
    Quotes,
    Context.Of[Nothing]
  ): Expr[?] = {
    def handleField(source: Structure.Named, field: Transformation.Field[Nothing])(using Sources, Sources.Scope, Quotes) =
      field match {
        case Field.FromSource(srcName, transformation) =>
          runTransformation(StructuredValue.of(source, primary).fieldValue(srcName), transformation)
        case Field.FromModifier(modifier) =>
          modifier match {
            case Configured.NamedSpecific.Add(valueStructure = struct, value = value) =>
              val source = Sources.current.get(value)
              StructuredValue.of(struct, source).fieldValue(struct.fieldName)
            case Configured.NamedSpecific.Compute(valueStructure = struct, fn = fn) =>
              Sources.current.get(fn) match {
                case '{ $fn: (src => out) } =>
                  '{
                    val computed = $fn(${ primary.asExprOf[src] })
                    ${
                      val computedValue = 'computed
                      StructuredValue.of(struct, computedValue).fieldValue(struct.fieldName)
                    }
                  }
              }
          }
      }

    Sources.current.withPrimary(primary) {
      transformation match {
        case Transformation.Named(source, fields, namesTpe, valuesTpe) =>
          (namesTpe, valuesTpe).runtimeChecked match {
            case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
              val args = fields.map((_, field) => handleField(source, field))
              val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
              '{ $recreated: NamedTuple[names, values] }
          }
        case Transformation.Tuple(source, fields, outputTpe) =>
          (source.tpe, outputTpe).runtimeChecked match {
            case '[source] -> '[output] =>
              val exprs = fields.map {
                case (idx, transformation) =>
                  runTransformation(StructuredValue.of(source, primary).elementValue(idx), transformation)
              }
              Expr.ofTupleFromSeq(exprs.toVector).asExprOf[output]
          }
        case Transformation.Optional(source, paramTransformation, outputTpe) =>
          (source.tpe, outputTpe).runtimeChecked match {
            case ('[Option[a]], '[Option[out]]) =>
              val optValue = primary.asExprOf[Option[a]]
              '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
          }

        case Transformation.EitherLike(source, left, right, outputTpe) =>
          (source.tpe, outputTpe).runtimeChecked match {
            case ('[scala.Either[e, a]], '[scala.Either[outE, outA]]) =>
              val eitherValue = primary.asExprOf[scala.Either[e, a]]
              '{
                $eitherValue match
                  case Left(value)  => Left(${ runTransformation('value, left).asExprOf[outE] })
                  case Right(value) => Right(${ runTransformation('value, right).asExprOf[outA] })
              }
          }

        case Transformation.ConfedUp(config) =>
          config match {
            case Configured.Update(fn = fn) =>
              Sources.current.get(fn) match { case '{ $fn: (src => out) } => '{ $fn(${ primary.asExprOf[src] }) } }

            case Configured.Sequence(tpe, source, unwrappedDest) =>
              Context.current match {
                case ctx: Context.PossiblyFallible[f] =>
                  given Type[f] = ctx.wrapperType.wrapper
                  val mode = ctx.mode.asExprOf[Mode.FailFast[f]]
                  val prim = StructuredValue.of(source, primary)
                  val fields = (0 until source.elements.size).map(idx =>
                    new FieldValue.Wrapped[f](
                      idx,
                      source.elements(idx).asInstanceOf[Structure.Wrapped[f]].wrapped.tpe,
                      prim.elementValue(idx).asExprOf[f[Any]]
                    )
                  )
                  val res = unwrappedDest match {
                    case '[dest] =>
                      ProductBinder.nestFlatMapsAndConstruct[f, dest](mode, Nil, fields.toList, ProductConstructor.Tuple(source))
                  }
                  res
                case _ => ???
              }
          }

        case Transformation.IterLike(source, paramTransformation, factory, outputTpe) =>
          (source.tycon, outputTpe, primary).runtimeChecked match {
            case (
                  '[type coll[a]; coll],
                  '[Iterable[elem]],
                  '{ $srcValue: Iterable[srcElem] }
                ) =>
              val f = factory.asExprOf[Factory[elem, coll[elem]]]
              '{
                $srcValue
                  .map[elem](srcElem => ${ runTransformation('srcElem, paramTransformation).asExprOf[elem] })
                  .to[coll[elem]]($f)
              }
          }

        case Transformation.MapLike(source, keyTransformation, valueTransformation, fac, outputTpe) =>
          (source.tycon, outputTpe, primary).runtimeChecked match {
            case (
                  '[type outMap[k, v]; outMap],
                  '[collection.Map[outKey, outValue]],
                  '{ $srcValue: collection.Map[srcKey, srcValue] }
                ) =>
              val factory = fac.asExprOf[Factory[(outKey, outValue), outMap[outKey, outValue]]]
              '{
                $srcValue
                  .map[outKey, outValue]((k, v) =>
                    (
                      ${ runTransformation('k, keyTransformation).asExprOf[outKey] },
                      ${ runTransformation('v, valueTransformation).asExprOf[outValue] }
                    )
                  )
                  .to[outMap[outKey, outValue]]($factory)
              }
          }

        case Transformation.Merged(mergees, fields, namesTpe, valuesTpe) =>
          (namesTpe, valuesTpe).runtimeChecked match {
            case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
              val args = fields.map {
                case (_, Transformation.Merged.Field.FromPrimary(source, field)) =>
                  handleField(source, field)
                case (_, field @ Transformation.Merged.Field.FromSecondary(name, ref, accessibleFrom, transformation)) =>
                  transformation match {
                    case Transformation.Leaf(output) =>
                      val value = Sources.current.get(ref)
                      StructuredValue.of(mergees(ref), value).fieldValue(name)
                    case merged: Transformation.Merged[Nothing] =>
                      given Sources = Sources.current.advance(mergees, field)
                      val nextPrimary = Sources.current.get(Sources.Ref.Primary)
                      runTransformation(nextPrimary, merged)
                  }
              }
              val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
              '{ $recreated: NamedTuple[names, values] }
          }

        case Transformation.Wrapped(source, wrapped, outputTpe, isHoisted) =>
          primary // TODO: ACTUAL IMPL!!!

        case Transformation.Leaf(_) => primary
      }
    }
  }

  extension (sources: Sources)
    private def advance(
      mergees: VectorMap[Sources.Ref, Structure.Named],
      field: Transformation.Merged.Field.FromSecondary[Nothing]
    )(using Sources.Scope, Quotes): Sources =
      field.accessibleFrom.foldLeft(sources) { (acc, ref) =>
        val struct = mergees(ref)
        val value = sources.get(ref)
        acc.updated(ref, StructuredValue.of(struct, value).fieldValue(field.name))
      }

  enum TransformationMode[F[x]] {
    def value: Expr[Mode[F]]

    case Accumulating(value: Expr[Mode.Accumulating[F]])
    case FailFast(value: Expr[Mode.FailFast[F]])
  }

  object TransformationMode {
    def create[F[x]: Type](expr: Expr[Mode[F]])(using Quotes): TransformationMode[F] =
      expr match
        case '{ $acc: Mode.Accumulating[F] } =>
          Accumulating(acc)
        case '{ $ff: Mode.FailFast[F] } =>
          FailFast(ff)
        case _ =>
          quotes.reflect.report.errorAndAbort(
            "Couldn't determine the transformation mode, make sure an instance of either Mode.FailFast[F] or Mode.Accumulating[F] is in implicit scope"
          )

    given Debug[TransformationMode[?]] with {
      def astify(self: TransformationMode[?])(using Quotes): AST =
        self match
          case Accumulating(value) => AST.Text("Accumulating")
          case FailFast(value)     => AST.Text("FailFast")

    }
  }
  private enum Value[F[x]] {
    final def wrapped[A](F: TransformationMode[F], tpe: Type[A])(using Quotes, Type[F]): Expr[F[A]] = {
      given Type[A] = tpe

      this match
        case Unwrapped(value) =>
          '{ ${ F.value }.pure[A](${ value.asExprOf[A] }) }
        case Wrapped(value) =>
          value.asExprOf[F[A]]
    }

    final def asFieldValue(index: Int, tpe: Type[?]): scala.Either[FieldValue.Unwrapped, FieldValue.Wrapped[F]] =
      this match {
        case unw: Unwrapped[F]   => Left(new FieldValue.Unwrapped(index, tpe, unw.value))
        case wrapped: Wrapped[F] => Right(new FieldValue.Wrapped(index, tpe, wrapped.value))
      }

    case Unwrapped(value: Expr[Any])
    case Wrapped(value: Expr[F[Any]])
  }
}
