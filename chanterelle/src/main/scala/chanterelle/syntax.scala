package chanterelle

import chanterelle.hidden.TupleModifier
import chanterelle.internal.EntryPoint

import scala.NamedTuple.*

extension [Tup <: AnyNamedTuple](self: Tup) {

  /**
   * Transforms a named tuple with one of the supported modifiers (see [[chanterelle.hidden.TupledModifier.Builder]])
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
   */
  transparent inline def transform(inline modifications: TupleModifier.Builder[Tup] => TupleModifier[Tup]*) =
    EntryPoint.run[Tup](self, modifications*)
}
