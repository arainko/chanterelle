package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Structure.*
import NamedTuple.*
import scala.collection.Factory

object Tuples {
  def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple
}

private[chanterelle] object Interpreter {
  def runTransformation(value: Expr[Any], transformation: ModifiableTransformation)(using Quotes): Expr[?] = {
    import quotes.reflect.*
    transformation match
      case t @ ModifiableTransformation.Named(source, fields) =>
        ((t.calculateNamesTpe, t.calculateValuesTpe): @unchecked) match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            val args = fields.map {
              case (name, ModifiableTransformation.OfField.FromModifier(modifier)) =>
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

              case (_, ModifiableTransformation.OfField.FromSource(idx, transformation, _)) =>
                runTransformation(StructuredValue.of(source, value).fieldValue(idx), transformation)
            }
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }
      case t @ ModifiableTransformation.Tuple(source, fields) =>
        (source.tpe, t.calculateTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.zipWithIndex.map {
              case (transformation, idx) =>
                runTransformation(StructuredValue.of(source, value).elementValue(idx), transformation)
            }
            Expr.ofTupleFromSeq(exprs).asExprOf[output]
        }
      case t @ ModifiableTransformation.Optional(source, paramTransformation) =>
        (source.tpe, t.calculateTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }

      case ModifiableTransformation.ConfedUp(config) =>
        config match
          case update: Configured.Update =>
            update.fn match {
              case '{ $fn: (src => out) } =>
                '{ $fn(${ value.asExprOf[src] }) }
            }

      case ModifiableTransformation.Iter(source, paramTransformation) =>
        paramTransformation.calculateTpe match {
          case '[elem] =>
            value match {
              case '{ $srcValue: Iterable[srcElem] } =>
                source.tycon match {
                  case '[type coll[a]; coll] =>
                    val factory = Expr.summon[Factory[elem, coll[elem]]].getOrElse(report.errorAndAbort("No factory found"))
                        '{
                          $srcValue
                            .map[elem](srcElem => ${ runTransformation('srcElem, paramTransformation).asExprOf[elem] })
                            .to[coll[elem]]($factory)
                        }

                }
            }
        }

      case ModifiableTransformation.Map(source, keyTransformation, valueTransformation) =>
        (source.tycon, keyTransformation.calculateTpe, valueTransformation.calculateTpe, value): @unchecked match {
          case ('[type map[k, v]; map], '[key], '[value], '{ $srcValue: collection.Map[k, v] }) =>
            val factory = Expr.summon[Factory[(key, value), map[key, value]]].getOrElse(report.errorAndAbort("No factory found"))
            '{
              $srcValue
                .map[key, value]((k, v) =>
                  (
                    ${ runTransformation('k, keyTransformation).asExprOf[key] },
                    ${ runTransformation('v, valueTransformation).asExprOf[value] }
                  )
                )
                .to[map[key, value]]($factory)
            }
        }
      case ModifiableTransformation.Leaf(_) => value
  }
}
