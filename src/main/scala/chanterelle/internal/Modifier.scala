package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*
import chanterelle.TupleModifier
import chanterelle.internal.Path.Segment

enum Modifier derives Debug {
  def path: Path

  case Add(path: Path, fieldName: String, value: Expr[?])
  case Update(path: Path, tpe: Type[?], function: Expr[? => ?])
  case Remove(path: Path, fieldToRemove: String)
}

object Modifier {
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): List[Modifier] = {
    import quotes.reflect.*
    mods.map {
      case '{
            type selected <: AnyNamedTuple
            type newField
            type newFieldName <: String
            (builder: TupleModifier.Builder[tup]) => builder.add[selected](${ AsTerm(PathSelector(path)) })[newFieldName, newField]($value)
          } =>
        Modifier.Add(path, Type.valueOfConstant[newFieldName].getOrElse(report.errorAndAbort("name of field needs to be known")), value)

      // case '{
      //   type selected <: AnyNamedTuple
      //   type newField <: AnyNamedTuple
      //   (a: TupleModifier.Builder[tup]) => a.compute[selected](${ AsTerm(PathSelector(path)) })[newField]($fn)
      // } =>
      //   val outputStructure = Structure.toplevel[newField].narrow[Structure.Named].getOrElse(report.errorAndAbort("Needs to be a named struct"))

      //   Modifier.Compute(path, outputStructure, fn.asInstanceOf) //TODO: get rid of cast MAYBE

      case '{ (builder: TupleModifier.Builder[tup]) =>
            builder.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn)
          } =>
        Modifier.Update(path, Type.of[newField], fn)

      case '{ (builder: TupleModifier.Builder[tup]) =>
            builder.remove[selected](${ AsTerm(PathSelector(path)) })
          } =>
        path.stripLast.collect { case (path, Path.Segment.Field(tpe, name)) => Modifier.Remove(path, name) }
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
