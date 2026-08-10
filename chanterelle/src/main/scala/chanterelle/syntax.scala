package chanterelle

import chanterelle.hidden.TupleModifier
import chanterelle.internal.EntryPoint

import scala.NamedTuple.*
import chanterelle.hidden.Tuples
import scala.annotation.nowarn
import chanterelle.hidden.Tuples.Mapped

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

type Traverse[Tup <: NamedTuple.AnyNamedTuple, F[_], G[_]] =
  G[NamedTuple.NamedTuple[NamedTuple.Names[Tup], Tuple.Map[Tuple.InverseMap[NamedTuple.DropNames[Tup], G], F]]]

type Sequence[Tup <: NamedTuple.AnyNamedTuple, F[_]] =
  Traverse[Tup, [a] =>> a, F]

// as all things in life - breaks down with unmatchables like opaque types (i.e. named tuple smh)
type InverseWrapped[X <: Tuple, F[_]] <: Tuple = X match {
  case F[x] *: t  => x *: InverseWrapped[t, F]
  case a *: t     => a *: InverseWrapped[t, F]
  case EmptyTuple => EmptyTuple
}

type SequenceSome[Tup <: NamedTuple.AnyNamedTuple, F[_]] =
  F[NamedTuple.NamedTuple[NamedTuple.Names[Tup], InverseWrapped[NamedTuple.DropNames[Tup], F]]]

type Seqd = SequenceSome[(int: Option[Int], str: Option[String], arrr: Option[(nest: String)]), Option]

val a: Option[(int: Int, str: String, arrr: (nest: String))] = ??? : Seqd
