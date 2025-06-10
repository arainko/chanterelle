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

  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*): Any = ${ runMacro[A]('tuple, 'mods) }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.* 

    val structure = Structure.toplevel[A]

    val mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))

    val modifiers = Modifier.parse(mods.toList)

    // val modifiers = List(
    //   Modifier.Add(Path.empty(Type.of[Int]), Structure.toplevel[(newField: Int)].narrow[Structure.Named].get, '{ (newField = 123) })
    // )

    val transformation = Transformation.fromStructure(structure)

    val modifiedTransformation = modifiers.foldLeft(transformation)((acc, mod) => acc.applyModifier(mod))

    // report.errorAndAbort(s"GOING IN ${modifiedTransformation.asInstanceOf[Transformation.Named].output.show}")

    val expr = Interpreter.runTransformation(tuple, modifiedTransformation)

    // report.errorAndAbort(expr.asTerm.show(using Printer.TreeShortCode))
    expr

    // modifiers.foreach(transformation.applyModifier)

    // Interpreter.run(tuple, transformation)

    // report.info(Debug.show(modifiers))

    // report.info(mods.map(expr => expr.asTerm.show(using Printer.TreeShortCode)).mkString(System.lineSeparator() * 2))
  }
}
