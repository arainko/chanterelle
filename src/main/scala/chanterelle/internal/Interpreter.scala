// package chanterelle.internal

// import scala.quoted.*
// import chanterelle.internal.Structure.*
// import NamedTuple.*
// import scala.collection.Factory

// object Tuples {
//   def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple
// }

// private[chanterelle] object Interpreter {
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

//   def runTransformation(value: Expr[Any], transformation: Transformation)(using Quotes): Expr[?] = {
//     transformation match
//       case Transformation.Named(source, output, fields) => ???
//       case Transformation.Tuple(source, output, fields) => ???
//       case Transformation.Optional(source, output, paramTransformation) =>
//         source.tpe match {
//           case '[Option[a]] =>
//             val optValue = value.asExprOf[Option[a]]
//             '{ $optValue.map[a](a => ${ runTransformation('a, paramTransformation).asExprOf[a] }) }
//         }
//       case Transformation.Collection(source, output, paramTransformation) =>
//         source.tpe match {
//           case '[type cc[a] <: Iterable[a]; cc[a]] =>
//             val optValue = value.asExprOf[cc[a]]
//             val factory = Expr.summon[Factory[a, cc[a]]].get
//             '{ $optValue.map[a](a => ${ runTransformation('a, paramTransformation).asExprOf[a] }).to($factory) }
//         }
//       case Transformation.Leaf(structure) => value
    
//   }
// }
