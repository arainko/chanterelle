package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Structure.*
import NamedTuple.*
import scala.collection.Factory
import chanterelle.internal.Transformation.OfField

object Tuples {
  def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple
}

private[chanterelle] object Interpreter {
//   def run(value: Expr[Any], transformation: Structure)(using Quotes): Expr[?] = {
//     transformation match {
//       case t @ Named(tpe, namesTpe, valuesTpe, path, fields) =>
//         ((namesTpe, valuesTpe): @unchecked) match {
//           case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
//             val args = fields.map { (name, struct) => run(value.accessNamedTupleFieldByName(name, t), struct) }.toVector
//             val recreated = Expr.ofTupleFromSeq(args).asExprOf[values]
//             '{ $recreated: NamedTuple[names, values] }
//         }

//       case tup @ Structure.Tuple(tpe, path, elements, isPlain) =>
//         val args = elements.zipWithIndex.map((t, idx) => run(value.accesFieldByIndex(idx, tup), t))
//         Expr.ofTupleFromSeq(args)
//       case Optional(tpe, path, paramStruct) =>
//         tpe match {
//           case '[Option[a]] =>
//             val optValue = value.asExprOf[Option[a]]
//             '{ $optValue.map[a](a => ${ run('a, paramStruct).asExprOf[a] }) }
//         }
//       case Collection(tpe, path, paramStruct) =>
//         tpe match {
//           case '[type cc[a] <: Iterable[a]; cc[a]] =>
//             val optValue = value.asExprOf[cc[a]]
//             val factory = Expr.summon[Factory[a, cc[a]]].get
//             '{ $optValue.map[a](a => ${ run('a, paramStruct).asExprOf[a] }).to($factory) }
//         }
//       // case Modified(tpe, path, modifier) =>
//       case Leaf(tpe, path) => value
//     }
//   }

  def runTransformation(value: Expr[Any], transformation: Transformation)(using Quotes): Expr[?] = {
    import quotes.reflect.*
    transformation match
      case Transformation.Named(source, output, fields) => 
        (output.calculateNamesTpe, output.calculateValuesTpe): @unchecked match {
          case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
            // quotes.reflect.report.errorAndAbort(Type.show[values])
            val args = fields.map {
              case (name, Transformation.OfField.FromModifier(modifier)) =>
                modifier match
                  case Modifier.Add(path, outputStructure, value) => 
                    value.accessNamedTupleFieldByName(name, outputStructure)
                  case Modifier.Compute(path, outputStructure, function) => ???
                  case Modifier.Update(path, function) => ???
                  case Modifier.Remove(path) => ???
                
              case (_, Transformation.OfField.FromSource(idx, transformation)) =>
                runTransformation(value.accessNamedTupleFieldByName(idx, output), transformation)
            }
            // report.errorAndAbort(Debug.show(transformation))
            val recreated = Expr.ofTupleFromSeq(args.toVector).asExprOf[values]
            '{ ($recreated: values): NamedTuple[names, values] }
        }
      case Transformation.Tuple(source, output, fields) => 
        (source.calculateTpe, output.calculateTpe): @unchecked match {
          case '[source] -> '[output] =>
            val exprs = fields.map {
              case OfField.FromSource(idx, transformation) => 
                runTransformation(value.accesFieldByIndex(idx, source), transformation)
              case OfField.FromModifier(modifier) =>
                ???
            }
            Expr.ofTupleFromSeq(exprs).asExprOf[output]
        }
      case Transformation.Optional(source, output, paramTransformation) =>
        (source.calculateTpe, output.calculateTpe): @unchecked match {
          case ('[Option[a]], '[Option[out]]) =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }) }
        }
      case Transformation.Collection(source, output, paramTransformation) =>
        (source.calculateTpe, output.calculateTpe): @unchecked match {
          case '[type cc[a] <: Iterable[a]; cc[a]] -> '[type ccOut[out] <: Iterable[out]; ccOut[out]] =>
            val optValue = value.asExprOf[cc[a]]
            val factory = Expr.summon[Factory[out, ccOut[out]]].get
            '{ $optValue.map[out](a => ${ runTransformation('a, paramTransformation).asExprOf[out] }).to($factory) }
        }
      case Transformation.Leaf(structure) => value
  }

}
