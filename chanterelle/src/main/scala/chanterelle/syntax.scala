package chanterelle

import chanterelle.hidden.TupleModifier
import chanterelle.internal.EntryPoint

import scala.NamedTuple.*
import chanterelle.hidden.Tuples
import scala.annotation.nowarn
import chanterelle.hidden.Tuples.Mapped

extension [Tup <: AnyNamedTuple](self: Tup) {

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

extension [Tup <: Tuple](self: Tup) {
  @nowarn("msg=the type test")
  inline def mapEach[U >: Tuple.Union[Tup], B](f: U => B): Tuple.Map[Tup, Tuples.Mapped[U, B]] =
    (self: Tup).map[Tuples.Mapped[U, B]](Tuples.Mapped(f))
}

extension [Names <: Tuple, Values <: Tuple, Tup <: NamedTuple.NamedTuple[Names, Values]](self: Tup) {
  @nowarn("msg=the type test")
  inline def mapEach[U >: Tuple.Union[NamedTuple.DropNames[Tup]], B](
    f: U => B
  ): NamedTuple[Names, Tuple.Map[Values, Mapped[U, B]]] =
    (self: Tup).map[Tuples.Mapped[U, B]](Tuples.Mapped(f))
}
