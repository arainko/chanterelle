package chanterelle.internal

import chanterelle.TupleModifier

import scala.quoted.*

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    import quotes.reflect.*
    val struct = Structure.toplevel[A]
    report.info(Debug.show(struct))
    '{}
  }

  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*): Any = ${ runMacro[A]('tuple, 'mods) }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.* 

    val structure = Structure.toplevel[A]

    val mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))

    val modifiers = Modifier.parse(mods.toList)

    val transformation = ModifiableTransformation.create(structure)

    val modifiedTransformation = modifiers.foldLeft(transformation)((acc, mod) => acc.applyModifier(mod))

    val errorlessTransformation = modifiedTransformation.refine.toOption.get

    val interpratableTransformation = InterpretableTransformation.create(errorlessTransformation)

    Interpreter.runTransformation(tuple, interpratableTransformation)

  }
}
