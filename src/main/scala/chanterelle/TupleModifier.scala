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
    def put[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[Value <: AnyNamedTuple](
      value: Value
    ): TupleModifier[Tup]

    def compute[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[Value <: AnyNamedTuple](
      f: Selected => Value
    ): TupleModifier[Tup]

    def update[Selected](selector: Selector ?=> Tup => Selected)[NewField](f: Selected => NewField): TupleModifier[Tup]

    def remove[Selected](selector: Selector ?=> Tup => Selected): TupleModifier[Tup]
  }

}
