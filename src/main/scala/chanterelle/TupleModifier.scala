package chanterelle

import scala.NamedTuple.*

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

opaque type TupleModifier[Tup] = Unit

object TupleModifier {

  sealed trait Builder[Tup] {
    def add[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[NewFieldName <: String, NewField](
      value: NamedTuple[Tuple1[NewFieldName], Tuple1[NewField]]
    ): TupleModifier[Tup]

    def compute[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[NewFieldName <: String, NewField](
      f: Selected => NamedTuple[Tuple1[NewFieldName], Tuple1[NewField]]
    ): TupleModifier[Tup]

    def update[Selected](selector: Selector ?=> Tup => Selected)[NewField](f: Selected => NewField): TupleModifier[Tup]

    def remove[Selected](selector: Selector ?=> Tup => Selected): TupleModifier[Tup]
  }

}
