package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Structure.*
import NamedTuple.*
import scala.collection.Factory

private[chanterelle] object Interpreter {

  def runTransformation(value: Expr[Any], transformation: InterpretableTransformation)(using Quotes): Expr[?] = {
    transformation match
      case InterpretableTransformation.Named(source, fields, namesTpe, valuesTpe) =>
        ((namesTpe, valuesTpe): @unchecked) match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            val args = fields.map {
              case (_, InterpretableTransformation.OfField.FromModifier(modifier)) =>
                modifier match {
                  case Configured.NamedSpecific.Add(valueStructure = struct, value = value) =>
                    StructuredValue.of(struct, value).fieldValue(struct.fieldName)
                  case Configured.NamedSpecific.Compute(valueStructure = struct, fn = fn) =>
                    fn match {
                      case '{ $fn: (src => out) } =>
                        '{
                          val computed = $fn(${ value.asExprOf[src] })
                          ${
                            val computedValue = 'computed
                            StructuredValue.of(struct, computedValue).fieldValue(struct.fieldName)
                          }
                        }
                    }
                }

              case (_, InterpretableTransformation.OfField.FromSource(idx, transformation)) =>
                runTransformation(StructuredValue.of(source, value).fieldValue(idx), transformation)
            }
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }
      case InterpretableTransformation.Tuple(source, fields, outputTpe) =>
        (source.tpe, outputTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.map {
              case (idx, transformation) =>
                runTransformation(StructuredValue.of(source, value).elementValue(idx), transformation)
            }
            Expr.ofTupleFromSeq(exprs.toVector).asExprOf[output]
        }
      case InterpretableTransformation.Optional(source, paramTransformation, outputTpe) =>
        (source.tpe, outputTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }

      case InterpretableTransformation.ConfedUp(config) =>
        config match
          case update: Configured.Update =>
            update.fn match {
              case '{ $fn: (src => out) } =>
                '{ $fn(${ value.asExprOf[src] }) }
            }

      case InterpretableTransformation.Iter(source, paramTransformation, factory, outputTpe) =>
        outputTpe match {
          case '[Iterable[elem]] =>
            value match {
              case '{ $srcValue: Iterable[srcElem] } =>
                source.tycon match {
                  case '[type coll[a]; coll] =>
                    val f = factory.asExprOf[Factory[elem, coll[elem]]]
                    '{
                      $srcValue
                        .map[elem](srcElem => ${ runTransformation('srcElem, paramTransformation).asExprOf[elem] })
                        .to[coll[elem]]($f)
                    }
                }
            }
        }

      case InterpretableTransformation.Map(source, keyTransformation, valueTransformation, fac, outputTpe) =>
        (source.tycon, outputTpe, value): @unchecked match {
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
      case InterpretableTransformation.Leaf(_) => value
  }
}
