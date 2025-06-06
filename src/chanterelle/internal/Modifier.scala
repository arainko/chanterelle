package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*
import chanterelle.TupleModifier

enum Modifier {
  case Add(path: Path, value: Expr[? <: AnyNamedTuple])
  case Compute(path: Path, function: Expr[? <: AnyNamedTuple => ? <: AnyNamedTuple])
}

object Modifier {
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): List[Modifier] = {
    import quotes.reflect.*
    mods.map {
      case '{
        type selected <: AnyNamedTuple
        // type newField <: AnyNamedTuple
        (a: TupleModifier.Builder[tup]) => a.add[selected]($selector)[newField]($value) 
      } => 
        val path = PathSelector.unapply(selector.asTerm).value
        Modifier.Add(path, value.asInstanceOf)

      case '{
        type selected <: AnyNamedTuple
        // type newField <: AnyNamedTuple
        (a: TupleModifier.Builder[tup]) => a.compute[selected]($selector)[newField]($fn) 
      } => 
        val path = PathSelector.unapply(selector.asTerm).value
        Modifier.Compute(path, fn.asInstanceOf) //TODO: get rid of cast MAYBE

      case other => report.errorAndAbort("fucky wucky:")
    }
  }
}
