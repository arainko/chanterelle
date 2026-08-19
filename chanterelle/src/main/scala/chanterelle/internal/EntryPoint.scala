package chanterelle.internal

import chanterelle.hidden.TupleModifier

import scala.annotation.publicInBinary
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion
import chanterelle.internal.Context.Total
import chanterelle.internal.Context.PossiblyFallible

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
      given Context.Any = Context.create
      structure = Structure.toplevel[A]
      mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))
      plan = Plan.create(structure)
      given Span = Span.ofMacroExpansion
      builder @ given Sources.Builder = Sources.newBuilder
      modifiers <- Modifier.parse(mods.toList).leftMap(ErrorsWithSpan)
      given Span = Span.minimalAvailable(modifiers.map(_.span))
      refinedPlan <- modifiers
        .foldLeft[Plan[Err]](plan)((transformation, mod) => transformation.applyModifier(mod))
        .refine
        .leftMap(ErrorsWithSpan)
      given Sources = builder.build
      expr <- Context.current match {
        case ctx @ given Context.Total.type =>
          Transformation
            .create(refinedPlan)
            .leftMap(err => ErrorsWithSpan(err :: Nil))
            .map(Interpreter.runTransformation(tuple, _))

        case ctx @ given Context.PossiblyFallible[f] =>
          Transformation
            .create(refinedPlan)
            .leftMap(err => ErrorsWithSpan(err :: Nil))
            .map(FallibleInterpreter.run(_, tuple))
      }
    } yield expr

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
