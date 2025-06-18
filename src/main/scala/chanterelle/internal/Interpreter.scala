package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Structure.*
import NamedTuple.*
import scala.collection.Factory
import chanterelle.internal.Transformation.OfField
import chanterelle.internal.Tuples.valuesOf

object Tuples {
  def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple
}

private[chanterelle] object Interpreter {
  def runTransformation(value: Expr[Any], transformation: Transformation)(using Quotes): Expr[?] = {
    import quotes.reflect.*
    transformation match
      case t @ Transformation.Named(source, fields) =>
        ((t.calculateNamesTpe, t.calculateValuesTpe): @unchecked) match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            val args = fields.map {
              case (
                    name,
                    Transformation.OfField.FromModifier(
                      Transformation.NamedSpecificConfigured.Add(valueStructure = struct, value = value)
                    )
                  ) =>

                StructuredValue.of(struct, value).fieldValue(name)

              case (_, Transformation.OfField.FromSource(idx, transformation)) =>
                runTransformation(StructuredValue.of(source, value).fieldValue(idx), transformation)
            }
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }
      case t @ Transformation.Tuple(source, fields) =>
        (source.tpe, t.calculateTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.zipWithIndex.map {
              case OfField.FromSource(idx, transformation) -> _ =>
                runTransformation(StructuredValue.of(source, value).elementValue(idx), transformation)
              case OfField.FromModifier(
                    Transformation.NamedSpecificConfigured.Add(valueStructure = struct, value = value)
                  ) -> _ =>
                ??? // TODO: Come up with a way that woul reject this from the AST at compiletime

            }
            Expr.ofTupleFromSeq(exprs).asExprOf[output]
        }
      case t @ Transformation.Optional(source, paramTransformation) =>
        (source.tpe, t.calculateTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }

      case Transformation.ConfedUp(config) =>
        config match
          case update: Transformation.Configured.Update =>
            update.fn match {
              case '{ $fn: (src => out) } =>
                '{ $fn(${ value.asExprOf[src] }) }
            }

      case t @ Transformation.Collection(source, paramTransformation) =>
        value match {
          case '{
                type a
                type srcColl <: Iterable[a]
                $coll: srcColl
              } =>
            report.errorAndAbort(
              s"God damn this thing why is this not decomposing into the type constructor ffs ${Type.show[srcColl]}"
            )

          // (source.tpe, t.calculateTpe): @unchecked match {
          //   case '[type ccSource <: Iterable[a]; ccSource] -> '[outParam] =>
          //     val optValue = value.asExprOf[ccSource]
          //     val factory = Expr.summon[Factory[out, ccOut[out]]].get
          //     '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }).to($factory) }
        }
      case Transformation.Leaf(_) => value
  }
}
