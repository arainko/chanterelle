package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*
import chanterelle.TupleModifier

enum Modifier derives Debug {
  def path: Path

  case Add(path: Path, outputStructure: Structure.Named, value: Expr[? <: AnyNamedTuple])
  // case Compute(path: Path, outputStructure: Structure.Named, function: Expr[? <: AnyNamedTuple => ? <: AnyNamedTuple])
  // case Update(path: Path, function: Expr[? => ?])
  // case Remove(path: Path)
}

object Modifier {
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): List[Modifier] = {
    import quotes.reflect.*
    mods.map {
      case '{
        type selected <: AnyNamedTuple
        type newField <: AnyNamedTuple
        (a: TupleModifier.Builder[tup]) => a.add[selected](${ AsTerm(PathSelector(path)) })[newField]($value) 
      } => 
        val outputStructure = Structure.toplevel[newField].narrow[Structure.Named].getOrElse(report.errorAndAbort("Needs to be a named struct"))
        Modifier.Add(path, outputStructure, value)

      // case '{
      //   type selected <: AnyNamedTuple
      //   type newField <: AnyNamedTuple
      //   (a: TupleModifier.Builder[tup]) => a.compute[selected](${ AsTerm(PathSelector(path)) })[newField]($fn) 
      // } => 
      //   val outputStructure = Structure.toplevel[newField].narrow[Structure.Named].getOrElse(report.errorAndAbort("Needs to be a named struct"))

      //   Modifier.Compute(path, outputStructure, fn.asInstanceOf) //TODO: get rid of cast MAYBE

      // case '{
      //   (a: TupleModifier.Builder[tup]) => a.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn) 
      // } => 
      //   Modifier.Update(path, fn)

      // case '{
      //   type selected <: AnyNamedTuple
      //   (a: TupleModifier.Builder[tup]) => a.remove[selected](${ AsTerm(PathSelector(path)) })
      // } => 
      //   Modifier.Remove(path)

      case other => report.errorAndAbort("fucky wucky:")
    }
  }

  private object AsTerm {
    def unapply(expr: Expr[Any])(using Quotes): Some[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }
}
