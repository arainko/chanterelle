package chanterelle.internal

import chanterelle.internal.Sources.Ref
import chanterelle.internal.Structure.*
import chanterelle.internal.Transformation.Field

import scala.collection.Factory
import scala.collection.immutable.VectorMap
import scala.quoted.*

import NamedTuple.*

private[chanterelle] object Interpreter {

  def runTransformation(primary: Expr[Any], transformation: Transformation)(using secondary: Sources, quotes: Quotes): Expr[?] = {
    def handleField(source: Structure.Named, field: Transformation.Field)(using secondary: Sources, quotes: Quotes) =
      field match {
        case Field.FromSource(srcName, transformation) =>
          runTransformation(StructuredValue.of(source, primary).fieldValue(srcName), transformation)
        case Field.FromModifier(modifier) =>
          modifier match {
            case Configured.NamedSpecific.Add(valueStructure = struct, value = value) =>
              StructuredValue.of(struct, value).fieldValue(struct.fieldName)
            case Configured.NamedSpecific.Compute(valueStructure = struct, fn = fn) =>
              fn match {
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
            fn match { case '{ $fn: (src => out) } => '{ $fn(${ primary.asExprOf[src] }) } }
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
        Sources.current.withPrimary(primary) {
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
        }

      case Transformation.Leaf(_) => primary
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
