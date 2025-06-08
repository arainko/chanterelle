package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Structure.*
import NamedTuple.*
import scala.collection.Factory

object Tuples {
  def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple
}

private[chanterelle] object Interpreter {
  def run(value: Expr[Any], transformation: Structure)(using Quotes): Expr[?] = {
    transformation match {
      case Named(tpe, namesTpe, valuesTpe, path, fields) =>
        // any other configuration and this falls apart with a MatchError...
        tpe match {
          case '[type tpe <: NamedTuple.AnyNamedTuple; tpe] =>
            ((Type.of[NamedTuple.DropNames[tpe]], Type.of[NamedTuple.Names[tpe]]): @unchecked) match {
              case ('[type values <: scala.Tuple; values], '[type names <: scala.Tuple; names]) =>
                value match {
                  case '{
                        type upper <: NamedTuple[names, values]
                        $v: upper
                      } =>
                    val values = Type.of[values]
                    val castedValue = '{ Tuples.valuesOf($v) }
                    val tup = Structure.Tuple(values, path, fields.values.toVector, true)
                    val args = fields.zipWithIndex.map {
                      case ((_, t), idx) => run(castedValue.accesFieldByIndex(idx, tup), t)
                    }.toVector
                    val recreated = Expr.ofTupleFromSeq(args).asExprOf[values]
                    '{ $recreated: NamedTuple[names, values] }
                }

            }

        }
      case tup @ Structure.Tuple(tpe, path, elements, isPlain) =>
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
