package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint


extension [Tup <: AnyNamedTuple](self: Tup) {
  transparent inline def modify[A <: Tup](inline modifications: TupleModifier.Builder[Tup] => TupleModifier[A]*) = ???

  inline def showStruct: Unit = EntryPoint.struct[Tup]
}
