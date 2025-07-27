package chanterelle.hidden

import scala.NamedTuple.*

opaque type TupleModifier[Tup] = Unit

object TupleModifier {

  sealed trait Builder[Tup] {

    /**
     * Puts a new field inside the selected named tuple
     * {{{
     * val tup = (anotherField = (field1 = 123))
     * val expected = (anotherField = (field1 = 123, newField = "garmanbozia"))
     *
     * val actual = tup.transform(_.put(_.anotherField)((newField = "garmanbozia")))
     *
     * assertEquals(actual, expected)
     * }}}
     */
    def put[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[Value <: AnyNamedTuple](
      value: Value
    ): TupleModifier[Tup]

    /**
     * Puts a new field inside the selected named tuple by evaluating a function over the selection
     * {{{
     * val tup = (anotherField = (field1 = 123))
     * val expected = (anotherField = (field1 = 123, newField = 123 + 23))
     *
     * val actual = tup.transform(_.compute(_.anotherField)(value => (newField = value.field1 + 23)))
     *
     * assertEquals(actual, expected)
     * }}}
     */
    def compute[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected)[Value <: AnyNamedTuple](
      f: Selected => Value
    ): TupleModifier[Tup]

    /**
     * Updates a value under the selected path
     * {{{
     * val tup = (anotherField = (field1 = 123))
     * val expected = (anotherField = (field1 = 124))
     *
     * val actual = tup.transform(_.update(_.anotherField.field1)(_ + 1))
     *
     * assertEquals(actual, expected)
     * }}}
     */
    def update[Selected](selector: Selector ?=> Tup => Selected)[NewField](f: Selected => NewField): TupleModifier[Tup]

    /**
     * Removes the selected field
     * {{{
     * val tup = (anotherField = (field1 = 123, field2 = 123))
     * val expected = (anotherField = (field1 = 123))
     * 
     * val actual = tup.transform(_.remove(_.anotherField.field2))
     *
     * assertEquals(actual, expected)
     * }}}
     */
    def remove[Selected](selector: Selector ?=> Tup => Selected): TupleModifier[Tup]
  }

}
