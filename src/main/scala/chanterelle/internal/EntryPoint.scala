package chanterelle.internal

import chanterelle.TupleModifier

import scala.quoted.*

object EntryPoint {
  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*): Any = ${
    runMacro[A]('tuple, 'mods)
  }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.*

    val transformation = for {
      structure = Structure.toplevel[A]
      mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))
      transformation = Transformation.create(structure)
      modifiers <- Modifier.parse(mods.toList)
      modifiedTransformation = modifiers.foldLeft(transformation)((transformation, mod) => transformation.applyModifier(mod))
      refinedTransformation <- modifiedTransformation.refine
      interpretableTransformation <- InterpretableTransformation.create(refinedTransformation).left.map(_ :: Nil)
    } yield Interpreter.runTransformation(tuple, interpretableTransformation)

    transformation match {
      //TODO: add proper printing under the proper spans, calculating a span for accumulated errors, all that fun stuff...
      case Left(errors) => report.errorAndAbort(errors.map(_.render).mkString(System.lineSeparator()))
      case Right(transformation) => transformation
    }
  }
}
