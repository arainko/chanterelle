package chanterelle

import chanterelle.internal.EntryPoint

import scala.NamedTuple.*

extension [Tup <: AnyNamedTuple](self: Tup) {
  transparent inline def transform(inline modifications: TupleModifier.Builder[Tup] => TupleModifier[Tup]*) =
    EntryPoint.run[Tup](self, modifications*)
}
