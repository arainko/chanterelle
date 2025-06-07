package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Transformation.*
import NamedTuple.*
import scala.collection.Factory

private[chanterelle] object Interpreter {
  def run(value: Expr[Any], transformation: Transformation)(using Quotes): Expr[?] = {
    transformation match {
      case Named(tpe, path, fields) => 
        (tpe) match {
          case ('[type tpe <: NamedTuple.AnyNamedTuple; tpe]) =>
            (Type.of[NamedTuple.DropNames[tpe]], Type.of[NamedTuple.Names[tpe]]) match {
              case ('[type values <: scala.Tuple; values], '[type names <: scala.Tuple; names]) =>
                val values = Type.of[NamedTuple.DropNames[tpe]]
                val castedValue = '{ $value.asInstanceOf[values] }
                val tup = Transformation.Tuple(values, path, fields.values.toVector, true)
                val args = fields.zipWithIndex.map { case ((_, t), idx) => run(castedValue.accesFieldByIndex(idx, tup), t) }.toVector
                val recreated = Expr.ofTupleFromSeq(args).asExprOf[values]
                '{ $recreated.asInstanceOf[NamedTuple[names, values]] }
            }
          
        }
      case tup @ Transformation.Tuple(tpe, path, elements, isPlain) => 
        val args = elements.zipWithIndex.map((t, idx) => run(value.accesFieldByIndex(idx, tup), t))
        Expr.ofTupleFromSeq(args)
      case Optional(tpe, path, paramStruct) => 
        tpe match {
          case '[Option[a]] =>
            val optValue = value.asExprOf[Option[a]]
            '{ $optValue.map[a](a => ${ run('a, paramStruct).asExprOf[a] }) }
        }
      case Collection(tpe, path, paramStruct) => 
        tpe match {
          case '[type cc[a] <: Iterable[a]; cc[a]] =>
            val optValue = value.asExprOf[cc[a]]
            val factory = Expr.summon[Factory[a, cc[a]]].get
            '{ $optValue.map[a](a => ${ run('a, paramStruct).asExprOf[a] }).to($factory) }
        }
      // case Modified(tpe, path, modifier) =>
      case Leaf(tpe, path) => value
    }
    
  }
}
