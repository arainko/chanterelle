package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*
import chanterelle.TupleModifier
import chanterelle.internal.Path.Segment

enum Modifier derives Debug {
  def path: Path

  case Add(path: Path, outputStructure: Structure.Named, value: Expr[? <: AnyNamedTuple])
  // case Compute(path: Path, outputStructure: Structure.Named, function: Expr[? <: AnyNamedTuple => ? <: AnyNamedTuple])
  case Update(path: Path, tpe: Type[?], fieldToUpdate: String, function: Expr[? => ?])
  case Remove(path: Path, fieldToRemove: String)
}

object Modifier {
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): List[Modifier] = {
    import quotes.reflect.*
    mods.map {
      case '{
        type selected <: AnyNamedTuple
        type newField <: AnyNamedTuple
        (builder: TupleModifier.Builder[tup]) => builder.add[selected](${ AsTerm(PathSelector(path)) })[newField]($value) 
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

      case '{
        (builder: TupleModifier.Builder[tup]) => builder.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn) 
      } => 
        path
          .stripLast
          .collect {
            case (path, Path.Segment.Field(name = name)) => Modifier.Update(path, Type.of[newField], name, fn)
          }
          .getOrElse(report.errorAndAbort("Needs to point to a field"))

      case '{
        (builder: TupleModifier.Builder[tup]) => builder.remove[selected](${ AsTerm(PathSelector(path)) })
      } => 
        path
          .stripLast
          .collect { case (path, Path.Segment.Field(tpe, name)) => Modifier.Remove(path, name) }
          .getOrElse(report.errorAndAbort("Needs to point to a field"))
          
      case other => report.errorAndAbort(s"Error parsing modifier: ${other.show}")
    }
  }

  private object AsTerm {
    def unapply(expr: Expr[Any])(using Quotes): Some[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }
}
