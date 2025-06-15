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
              case (name, Transformation.OfField.FromModifier(Transformation.Configured.Add(fieldName, outputStructure, valueStructure, value))) =>
                  value.accessNamedTupleFieldByName(name, valueStructure)
              case (name, Transformation.OfField.FromModifier(Transformation.Configured.Update(fn = fn))) =>
                  fn match {
                    case '{ $fn: (src => out) } => 
                      val fieldValue = value.accessNamedTupleFieldByName(name, source)

                      '{ $fn(${ fieldValue.asExprOf[src] }) }
                  }
                
              case (_, Transformation.OfField.FromSource(idx, transformation)) =>
                runTransformation(value.accessNamedTupleFieldByName(idx, source), transformation)
            }
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ $recreated: NamedTuple[names, values] }
        }
      case t @ Transformation.Tuple(source, fields) => 
        (source.tpe, t.calculateTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.map {
              case OfField.FromSource(idx, transformation) => 
                runTransformation(value.accesFieldByIndex(idx, source), transformation)
              case OfField.FromModifier(modifier) =>
                ???
            }
            Expr.ofTupleFromSeq(exprs).asExprOf[output]
        }
      case t @ Transformation.Optional(source, paramTransformation) =>
        (source.tpe, t.calculateTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }
      case t @ Transformation.Collection(source, paramTransformation) =>
        value match {
          case '{ 
            type a
            type srcColl <: Iterable[a]
            $coll: srcColl 
          } =>
            report.errorAndAbort(s"God damn this thing why is this not decomposing into the type constructor ffs ${Type.show[srcColl]}")

        
        // (source.tpe, t.calculateTpe): @unchecked match {
        //   case '[type ccSource <: Iterable[a]; ccSource] -> '[outParam] =>
        //     val optValue = value.asExprOf[ccSource]
        //     val factory = Expr.summon[Factory[out, ccOut[out]]].get
        //     '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }).to($factory) }
        }
      case Transformation.Leaf(_) => value
  }

  type CollOf[F[a] <: Iterable, A] = F[A]

}
