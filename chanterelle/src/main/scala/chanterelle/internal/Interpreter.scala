package chanterelle.internal

import chanterelle.internal.Structure.*

import scala.collection.Factory
import scala.quoted.*

import NamedTuple.*
import chanterelle.internal.Transformation.Field
import chanterelle.internal.Sources.Ref

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
        (namesTpe, valuesTpe): @unchecked match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            val args = fields.map((_, field) => handleField(source, field))
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }
      case Transformation.Tuple(source, fields, outputTpe) =>
        (source.tpe, outputTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.map {
              case (idx, transformation) =>
                runTransformation(StructuredValue.of(source, primary).elementValue(idx), transformation)
            }
            Expr.ofTupleFromSeq(exprs.toVector).asExprOf[output]
        }
      case Transformation.Optional(source, paramTransformation, outputTpe) =>
        (source.tpe, outputTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = primary.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }

      case Transformation.EitherLike(source, left, right, outputTpe) =>
        (source.tpe, outputTpe): @unchecked match {
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
        (source.tycon, outputTpe, primary): @unchecked match {
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
        (source.tycon, outputTpe, primary): @unchecked match {
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

      case m @ Transformation.Merged(source, mergees, fields, namesTpe, valuesTpe) =>
        println(Debug.show(m))
        (namesTpe, valuesTpe): @unchecked match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            val args = fields.map {
              case (_, Transformation.Merged.Field.FromPrimary(field)) =>
                handleField(source, field)
              case (_, Transformation.Merged.Field.FromSecondary(name, ref, accessibleFrom, transformation)) =>
                transformation match {
                  case Transformation.Leaf(output) =>
                    val value = secondary.get(ref)
                    StructuredValue.of(mergees(ref), value).fieldValue(name)
                  case t: Transformation.Merged =>
                    given Sources = accessibleFrom.foldLeft(secondary) { (acc, ref) =>
                      val struct = if ref == Sources.Ref.Primary then source else mergees(ref)
                      val value = if ref == Sources.Ref.Primary then primary else secondary.get(ref)
                      println(ref)
                      println(Debug.show(struct))
                      acc.updated(ref, StructuredValue.of(struct, value).fieldValue(name))
                    }
                    runTransformation(primary, t)
                }
            }
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }

      case Transformation.Leaf(_) => primary
    }
  }
}
