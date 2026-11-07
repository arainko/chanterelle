package chanterelle

import chanterelle.hidden.Tuples.Mapped
import chanterelle.hidden.{ TupleModifier, Tuples }
import chanterelle.internal.EntryPoint

import scala.NamedTuple.*
import scala.annotation.nowarn

extension [Tup <: AnyNamedTuple | Tuple](self: Tup) {

  /**
   * Transforms a named tuple with one of the supported modifiers
   *
   * {{{
   * val value = (field1 = 1, nestedField = (field = 2, optionalField = Some(3)))
   *
   * value.transform(
   *   _.put(_.nestedField)((newField = 4)), // creates a new field under 'nestedField'
   *   _.update(_.nestedField.optionalField.element)(_ + 1) // updates the value inside `optionalField` if it is a Some
   *   _.remove(_.field1) // completely removes 'field1'
   * )
   * // evaluates to: (nestedField = (field = 2, optionalField = Some(4), newField = 4))
   * // and is typed as: (nestedField : (field : Int, optionalField : Option[Int], newField : Int))
   * }}}
   *
   * @see chanterelle.hidden.TupledModifier.Builder
   */
  transparent inline def transform(inline modifications: TupleModifier.Builder[Tup] => TupleModifier[Tup]*) =
    EntryPoint.run[Tup](self, modifications*)
}
