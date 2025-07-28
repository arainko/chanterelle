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
