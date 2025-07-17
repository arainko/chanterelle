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
  def parse[A](mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes): Either[List[ErrorMessage], List[Modifier]] = {
    import quotes.reflect.*
    val (errors, modifiers) = mods.partitionMap {
      // TODO: report an issue to dotty: not able to match with quotes if $value is of type NamedTuple[?, ?]
      case cfg @ '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.put[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
          Structure
            .toplevel[v]
            .narrow[Structure.Named.Singular]
            .toRight(ErrorMessage.ExpectedSingletonNamedTuple(Type.of[v], Span.fromExpr(cfg)))
            .map(valueStructure => Modifier.Add(path, valueStructure, value))        

      case cfg @ '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.compute[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
          Structure
            .toplevel[v]
            .narrow[Structure.Named.Singular]
            .toRight(ErrorMessage.ExpectedSingletonNamedTuple(Type.of[v], Span.fromExpr(cfg)))
            .map(valueStructure => Modifier.Compute(path, valueStructure, value)) 
        

      case '{ (builder: TupleModifier.Builder[tup]) =>
            builder.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn)
          } =>
        Right(Modifier.Update(path, Type.of[newField], fn))

      case cfg @ '{ (builder: TupleModifier.Builder[tup]) => builder.remove[selected](${ AsTerm(PathSelector(path)) }) } =>
        path
          .stripLast
          .collect {
            case (path, Path.Segment.Field(tpe, name))         => Right(Modifier.Remove(path, name))
            case (path, Path.Segment.TupleElement(tpe, index)) => Right(Modifier.Remove(path, index))
          }.getOrElse(Left(ErrorMessage.SelectorNeedsToPointToAField(path, Span.fromExpr(cfg))))

      case other => report.errorAndAbort(s"Error parsing modifier: ${other.asTerm.show(using Printer.TreeStructure)}")
    }

    if errors.isEmpty then Right(modifiers) else Left(errors)
  }

  private object AsTerm {
    def unapply(expr: Expr[Any])(using Quotes): Some[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }
}
