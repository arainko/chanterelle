package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint


extension [Tup <: AnyNamedTuple](self: Tup) {
  transparent inline def modify(inline modifications: TupleModifier.Builder[Tup] => TupleModifier[Tup]*) = 
    EntryPoint.run[Tup](self, modifications*)

  inline def showStruct: Unit = EntryPoint.struct[Tup]
}
