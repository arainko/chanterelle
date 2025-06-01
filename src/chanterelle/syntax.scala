package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint

sealed trait NamedTupleBuilder[Tpe] {
  def add[FieldTpe <: NamedTuple.AnyNamedTuple, FieldName <: String, A](
    selector: Selector ?=> Tpe => FieldTpe
  )(value: NamedTuple[FieldName *: EmptyTuple, A *: EmptyTuple]): NamedTupleBuilder[Tpe] = ???
}

extension [Labels <: Tuple, Values <: Tuple](self: NamedTuple[Labels, Values]) {
  inline def modify = {
    ???
  }

  inline def showStruct: Unit = EntryPoint.struct[NamedTuple[Labels, Values]]
}

object test {
  val a: (name: Int, age: Int, other: (something: (name: Int, age: Int))) = ???
}
