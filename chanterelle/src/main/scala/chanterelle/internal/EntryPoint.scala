package chanterelle.internal

import chanterelle.hidden.TupleModifier

import scala.annotation.publicInBinary
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion

object EntryPoint {
  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*): Any = ${
    runMacro[A]('tuple, 'mods)
  }

  @publicInBinary
  private[chanterelle] def runMacro[A: Type](
    tuple: Expr[A],
    modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]]
  )(using Quotes) = {
    import quotes.reflect.*

    val transformation = for {
      structure = Structure.toplevel[A]
      mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))
      transformation = Transformation.create(structure)
      given Span = Span.ofMacroExpansion
      modifiers <- Modifier.parse(mods.toList).leftMap(ErrorsWithSpan)
      given Span = Span.minimalAvailable(modifiers.map(_.span))
      modifiedTransformation = modifiers.foldLeft(transformation)((transformation, mod) => transformation.applyModifier(mod))
      refinedTransformation <- modifiedTransformation.refine.leftMap(ErrorsWithSpan)
      interpretableTransformation <-
        InterpretableTransformation.create(refinedTransformation).leftMap(err => ErrorsWithSpan(err :: Nil))
    } yield Interpreter.runTransformation(tuple, interpretableTransformation)

    transformation match {
      case Left((errors = errs, errorSpan = span)) => reportErrorsAndAbort(errs, span)
      case Right(transformation)                   => transformation
    }
  }

  // TODO: revisit this, it feels off
  private def reportErrorsAndAbort(errors: List[ErrorMessage], accumulatedErrorSpan: Span)(using Quotes) = {
    errors.groupBy {
      _.span match
        case None       => accumulatedErrorSpan
        case span: Span => span
    }
      .transform((_, errors) => errors.map(_.render).toList.distinct.mkString(System.lineSeparator))
      .foreach { (span, errorMessage) => quotes.reflect.report.error(errorMessage, span.toPosition) }

    throw new StopMacroExpansion
  }

  private def ErrorsWithSpan(using errorSpan: Span)(errors: List[ErrorMessage]) = (errors = errors, errorSpan = errorSpan)

}

import scala.compiletime.ops.string.*

object SnakeCaseConverter:

  type Uppercase = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"

  type ToLower[S <: String] <: String = S match
    case "A" => "a"
    case "B" => "b"
    case "C" => "c"
    case "D" => "d"
    case "E" => "e"
    case "F" => "f"
    case "G" => "g"
    case "H" => "h"
    case "I" => "i"
    case "J" => "j"
    case "K" => "k"
    case "L" => "l"
    case "M" => "m"
    case "N" => "n"
    case "O" => "o"
    case "P" => "p"
    case "Q" => "q"
    case "R" => "r"
    case "S" => "s"
    case "T" => "t"
    case "U" => "u"
    case "V" => "v"
    case "W" => "w"
    case "X" => "x"
    case "Y" => "y"
    case "Z" => "z"
    case _   => S

  // 1. Entry point: Handles the first character separately (to avoid leading underscores)
  type ToSnakeCase[S <: String] <: String = 
    Length[S] match
      case 0 => 
        ""
      case _ => 
        ToLower[Substring[S, 0, 1]] + SnakeStep[Substring[S, 1, Length[S]]]

  // 2. Recursive Step: Evaluates one character at a time and shrinks the string
  type SnakeStep[S <: String] <: String = 
    Length[S] match
      case 0 => 
        ""
      case _ => 
        Substring[S, 0, 1] match
          case Uppercase => 
            "_" + ToLower[Substring[S, 0, 1]] + SnakeStep[Substring[S, 1, Length[S]]]
          case _ => 
            Substring[S, 0, 1] + SnakeStep[Substring[S, 1, Length[S]]]

object test2 {
  val one = two

  def cos = two

  val two = 2
}
