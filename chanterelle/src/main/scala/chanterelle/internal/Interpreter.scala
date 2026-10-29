package chanterelle.internal

import chanterelle.internal.Configured.NamedSpecific
import chanterelle.internal.Sources.{ Ref, Scope }
import chanterelle.internal.Structure.*
import chanterelle.internal.Transformation.Field
import chanterelle.interop.Collection

import scala.annotation.unused
import scala.collection.immutable.VectorMap
import scala.quoted.*

import NamedTuple.*

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
          handleNamedSpecific(modifier)
      }

    Sources.current.withPrimary(primary) {
      transformation match {
        case Transformation.Named(source, fields, namesTpe, valuesTpe, _) =>
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
          }

        case mapped: Transformation.Mapped[f] =>
          @unused given Type[f] = mapped.source.wrapper.wrapper
          (mapped.source.wrapped.tpe, mapped.wrapped.outputTpe).runtimeChecked match {
            case '[a] -> '[b] =>
              val src = primary.asExprOf[f[a]]
              '{ ${ mapped.source.mappable }.map[a, b]($src, a => ${ runTransformation('a, mapped.wrapped).asExprOf[b] }) }
          }

        case Transformation.IterLike(
              source: Structure.Collection.Repr.IterLike[coll],
              paramTransformation,
              factory,
              outputTpe
            ) =>
          @unused given Type[coll] = source.tycon
          (source.element.tpe, paramTransformation.outputTpe).runtimeChecked match {
            case '[srcElem] -> '[elem] =>
              val f = factory match { case CollBuilder.Total(builder) => builder.unerase[elem, coll[elem]] }
              primary match {
                case '{ $srcValue: Iterable[`srcElem`] } =>
                  Logger.debug(s"coll[elem] is ${Type.show[coll[elem]]}")
                  '{
                    $srcValue
                      .map[elem](srcElem => ${ runTransformation('srcElem, paramTransformation).asExprOf[elem] })
                      .to[coll[elem]]($f)
                  }
                case _ =>
                  val srcValue = primary.asExprOf[coll[srcElem]]
                  val isColl = source.intoIter.unerase[srcElem, coll[srcElem]]
                  '{
                    $isColl
                      .iterator($srcValue)
                      .map[elem](srcElem => ${ runTransformation('srcElem, paramTransformation).asExprOf[elem] })
                      .to[coll[elem]]($f)
                  }
              }
          }

        case Transformation.MapLike(
              source: Structure.Collection.Repr.MapLike[outMap],
              keyTransformation,
              valueTransformation,
              fac,
              outputTpe
            ) =>
          @unused given Type[outMap] = source.tycon

          (outputTpe, primary) match {
            case (
                  '[`outMap`[outKey, outValue]],
                  '{ $srcValue: collection.Map[srcKey, srcValue] }
                ) =>
              val factory = fac match {
                case CollBuilder.Total(builder) => builder.unerase[(outKey, outValue), outMap[outKey, outValue]]
              }

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

            case _ =>
              (
                keyTransformation.outputTpe,
                valueTransformation.outputTpe,
                source.key.tpe,
                source.value.tpe
              ).runtimeChecked match {
                case (
                      '[outKey],
                      '[outValue],
                      '[srcKey],
                      '[srcValue]
                    ) =>
                  val srcValue = primary.asExprOf[outMap[srcKey, srcValue]]
                  val isColl = source.intoIter.unerase[(srcKey, srcValue), outMap[srcKey, srcValue]]
                  val factory = fac match {
                    case CollBuilder.Total(builder) => builder.unerase[(outKey, outValue), outMap[outKey, outValue]]
                  }
                  '{
                    $isColl
                      .iterator(
                        $srcValue
                      )
                      .map[(outKey, outValue)]((k, v) =>
                        (
                          ${ runTransformation('k, keyTransformation).asExprOf[outKey] },
                          ${ runTransformation('v, valueTransformation).asExprOf[outValue] }
                        )
                      )
                      .to[outMap[outKey, outValue]]($factory)
                  }
              }
          }

        case Transformation.Merged(mergees, fields, namesTpe, valuesTpe, _) =>
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
                    case merged: Transformation.Merged =>
                      given Sources = Sources.current.advance(mergees, field)
                      val nextPrimary = Sources.current.get(Sources.Ref.Primary)
                      runTransformation(nextPrimary, merged)
                  }
              }
              val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
              '{ $recreated: NamedTuple[names, values] }
          }

        case Transformation.Leaf(_) => primary
      }
    }
  }

  def handleNamedSpecific(modifier: NamedSpecific)(using Sources, Scope, Quotes): Expr[Any] =
    modifier match {
      case Configured.NamedSpecific.Add(valueStructure = struct, value = value) =>
        val source = Sources.current.get(value)
        StructuredValue.of(struct, source).fieldValue(struct.fieldName)
      case Configured.NamedSpecific.Compute(valueStructure = struct, fn = fn) =>
        Sources.current.get(fn) match {
          case '{ $fn: (src => out) } =>
            '{
              val computed = $fn(${ Sources.current.get(Sources.Ref.Primary).asExprOf[src] })
              ${
                val computedValue = 'computed
                StructuredValue.of(struct, computedValue).fieldValue(struct.fieldName)
              }
            }
        }
    }

  extension (sources: Sources)
    private def advance(
      mergees: VectorMap[Sources.Ref, Structure.Named],
      field: Transformation.Merged.Field.FromSecondary
    )(using Sources.Scope, Quotes): Sources =
      field.accessibleFrom.foldLeft(sources) { (acc, ref) =>
        val struct = mergees(ref)
        val value = sources.get(ref)
        acc.updated(ref, StructuredValue.of(struct, value).fieldValue(field.name))
      }
}
