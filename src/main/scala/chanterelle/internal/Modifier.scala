package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*
import chanterelle.TupleModifier

enum Modifier derives Debug {
  def path: Path

  case Add(path: Path, valueStructure: Structure.Named.Singular, value: Expr[?])
  case Compute(path: Path, valueStructure: Structure.Named.Singular, value: Expr[? => ?])
  case Update(path: Path, tpe: Type[?], function: Expr[? => ?])
  case Remove(path: Path, fieldToRemove: String | Int)
}

object Modifier {
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): List[Modifier] = {
    import quotes.reflect.*
    mods.map {
      // TODO: report an issue to dotty: not able to match with quotes if $value is of type NamedTuple[?, ?]
      case '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.put[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
        val valueStructure =
          Structure
            .toplevel[v]
            .narrow[Structure.Named.Singular]
            .getOrElse(report.errorAndAbort("Needs to be a named tuple of size 1"))
        Modifier.Add(path, valueStructure, value)

      case '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.compute[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
        val valueStructure =
          Structure
            .toplevel[v]
            .narrow[Structure.Named.Singular]
            .getOrElse(report.errorAndAbort("Needs to be a named tuple of size 1"))
        Modifier.Compute(path, valueStructure, value)

      case '{ (builder: TupleModifier.Builder[tup]) =>
            builder.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn)
          } =>
        Modifier.Update(path, Type.of[newField], fn)

      case '{ (builder: TupleModifier.Builder[tup]) => builder.remove[selected](${ AsTerm(PathSelector(path)) }) } =>
        path.stripLast.collect {
          case (path, Path.Segment.Field(tpe, name))         => Modifier.Remove(path, name)
          case (path, Path.Segment.TupleElement(tpe, index)) => Modifier.Remove(path, index)
        }.getOrElse(report.errorAndAbort("Needs to point to a field"))

      case other => report.errorAndAbort(s"Error parsing modifier: ${other.asTerm.show(using Printer.TreeStructure)}")
    }
  }

  private object AsTerm {
    def unapply(expr: Expr[Any])(using Quotes): Some[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }
}
