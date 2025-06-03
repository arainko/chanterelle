package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import scala.language.dynamics


/*
  Highlevel syntax:

  tuple.modify(
    _
      .add(_.level1.level2, (newField: 1))
      .add(_.level1, (nextNewField: (actualField: "asd")))
      .compute(_.level1.oldField.element, oldFieldValue => (newField: oldFieldValue + 1))
      .remove(_.level1.oldField)
      .modifyNames(_.toUpperCase)
    )
  )
   */

sealed trait NamedTupleBuilder[Tpe <: AnyNamedTuple] {
  def add[Selected <: AnyNamedTuple, NewField <: AnyNamedTuple](selector: Selector ?=> Tpe => Selected)(value: NewField): NamedTupleBuilder[Tpe]
  def compute[Selected, NewField <: AnyNamedTuple](selector: Selector ?=> Tpe => Selected)(f: Selected => NewField): NamedTupleBuilder[Tpe]
  def remove[Selected, NewField <: AnyNamedTuple](selector: Selector ?=> Tpe => Selected): NamedTupleBuilder[Tpe]
}

extension [Tup <: AnyNamedTuple](self: Tup) {
  transparent inline def modify(inline modification: NamedTupleBuilder[Tup] => NamedTupleBuilder[Tup]) = ???

  inline def showStruct: Unit = EntryPoint.struct[Tup]
}

object test {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int))]) = ???

  a.showStruct

  a.modify(
    _
    .add(_.other.element.something)((newField = 1))
    .compute(_.other.element.something)(a => (computedField = a.name + a.age))
  )
}
