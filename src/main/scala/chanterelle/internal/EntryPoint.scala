package chanterelle.internal

import scala.quoted.*
import chanterelle.TupleModifier

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    val struct = Transformation.toplevel[A]
    Logger.info("", struct)
    '{}
  }

  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*) = ${ runMacro[A]('tuple, 'mods) }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.* 

    val transformation = Transformation.toplevel[A]

    val mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))

    val modifiers = Modifier.parse(mods.toList)

    // modifiers.foreach(transformation.applyModifier)

    Interpreter.run(tuple, transformation)

    // report.info(Debug.show(modifiers))

    // report.info(mods.map(expr => expr.asTerm.show(using Printer.TreeShortCode)).mkString(System.lineSeparator() * 2))
  }
}
