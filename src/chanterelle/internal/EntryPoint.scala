package chanterelle.internal

import scala.quoted.*
import chanterelle.TupleModifier

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    val struct = Structure.toplevel[A]
    Logger.info("", struct)
    '{}
  }

  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*) = ${ runMacro[A]('tuple, 'mods) }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.* 

    val structure = Structure.toplevel[A]

    val mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))

    val modifiers = Modifier.parse(mods.toList)

    report.info(modifiers.toString())

    // report.info(mods.map(expr => expr.asTerm.show(using Printer.TreeShortCode)).mkString(System.lineSeparator() * 2))
    '{}
  }
}
