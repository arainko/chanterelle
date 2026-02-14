package chanterelle.hidden

import chanterelle.FieldName

import scala.NamedTuple.*
import scala.annotation.compileTimeOnly

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
    @compileTimeOnly("Only usable as part of the .transform DSL")
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
    @compileTimeOnly("Only usable as part of the .transform DSL")
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
    @compileTimeOnly("Only usable as part of the .transform DSL")
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
    @compileTimeOnly("Only usable as part of the .transform DSL")
    def remove[Selected](selector: Selector ?=> Tup => Selected): TupleModifier[Tup]

    /**
     * Renames fields according to the passed in FieldName to FieldName function (which needs to be known at compiletime).
     * {{{
     * val tup = (anotherField = (field1 = 123, field2 = 123)).transform(_.rename(_.toUpperCase))
     *
     * val expected = (ANOTHERFIELD = (FIELD1 = 123, FIELD2 = 123))
     *
     * assertEquals(tup, expected)
     * }}}
     *
     * The blast radius of the renaming function can be further controlled with '.local' and '.regional':
     * {{{
     *  val tup = (optField = Some((field = (lowerDown = 1))))
     *
     *  // point it at the named tuple inside the Option to only rename the 'toplevel' fields
     *  val actualLocal = tup.transform(_.rename(_.toUpperCase).local(_.optField.element))
     *  val expectedLocal = (optField = Some((FIELD = (lowerDown = 1))))
     *  assertEquals(actualLocal, expectedLocal)
     *
     *  // '.regional' makes it so it transforms all the of fields 'underneath' the path
     *  val actualRegional = tup.transform(_.rename(_.toUpperCase).regional(_.optField.element))
     *  val expectedRegional = (optField = Some((FIELD = (LOWERDOWN = 1))))
     *  assertEquals(actualRegional, expectedRegional)
     * }}}
     */
    @compileTimeOnly("Only usable as part of the .transform DSL")
    def rename(fieldName: FieldName => FieldName): TupleModifier[Tup] & Local[Tup] & Regional[Tup]

    /**
     * Deeply merges named tuples. 
     * 
     * Named tuples are merged by field name, fields from the named tuple we merge with (the mergee) take precedence, nested named tuples (that don't come from modifications) and merged values are recursed, other values get completely overwritten using the value from the mergee. 
     * 
     * {{{
     * val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
     * val mergee = (field2 = (level1Field3 = 5, level1Field2 = (anotherField = 6)))
     *
     * val actual = tup.transform(_.merge(mergee))
     *
     * val expected = (
     *    field1 = tup.field1,
     *    field2 = (
     *      level1Field1 = tup.field2.level1Field1,
     *      level1Field2 = (
     *        level2Field = tup.field2.level1Field2.level2Field,
     *        anotherField = mergee.field2.level1Field2.anotherField
     *      ),
     *      level1Field3 = mergee.field2.level1Field3
     *    )
     *  )
     *
     * assertEquals(actual, expected)
     * }}}
     *
     * Merges can be pointed at a specific nested named tuple with `.regional`
     * {{{
     * val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
     * val mergee = (level1Field2 = (anotherField = 6))
     *
     * val actual = tup.transform(_.merge(mergee).regional(_.field2))
     *
     * val expected = (
     *   field1 = tup.field1,
     *   field2 = (
     *     level1Field1 = tup.field2.level1Field1,
     *     level1Field2 = (
     *       level2Field = tup.field2.level1Field2.level2Field,
     *       anotherField = mergee.level1Field2.anotherField
     *     )
     *   )
     * )
     *
     * assertEquals(actual, expected)
     * }}}
     *
     * Certain limitations are imposed on merged values:
     *  * `.remove`, `.put` and `.compute` aren't currently supported (users can still 'cut through' merged values to access other nodes of the transformation)
     */
    @compileTimeOnly("Only usable as part of the .transform DSL")
    def merge[A <: NamedTuple.AnyNamedTuple](mergee: A): TupleModifier[Tup] & Regional[Tup]
  }

  sealed trait Local[Tup]

  object Local {
    extension [Tup](self: TupleModifier[Tup] & Local[Tup]) {
      @compileTimeOnly("Only usable as part of the .transform DSL")
      def local[Selected <: AnyNamedTuple](selector: Selector ?=> Tup => Selected): TupleModifier[Tup] = ???
    }
  }

  sealed trait Regional[Tup]

  object Regional {
    extension [Tup](self: TupleModifier[Tup] & Regional[Tup]) {
      @compileTimeOnly("Only usable as part of the .transform DSL")
      def regional[Selected](selector: Selector ?=> Tup => Selected): TupleModifier[Tup] = ???
    }
  }

}

@main def main = {
  import chanterelle.*

  val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))

  val mergee = (level1Field2 = (anotherField = 6))

  val actual = tup.transform(_.merge(mergee).regional(_.field2))

  val expected = (
    field1 = tup.field1,
    field2 = (
      level1Field1 = tup.field2.level1Field1,
      level1Field2 = (
        level2Field = tup.field2.level1Field2.level2Field,
        anotherField = mergee.level1Field2.anotherField
      )
    )
  )

  assert(actual == expected)

}
