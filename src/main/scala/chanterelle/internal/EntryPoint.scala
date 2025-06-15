package chanterelle.internal

import scala.quoted.*
import chanterelle.TupleModifier
import scala.collection.BuildFrom

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    import quotes.reflect.*
    Type.of[A] match {
      case tpe @ '[Iterable[param]] => 
        tpe.repr match {
          case AppliedType(tycon, _) =>
            tycon.asType match {
              case '[f] => 
                report.errorAndAbort(s"Ok, we matched: ${Type.show[f]}")
            }
        }
        // typeCon.tpe.asType match {
        //   case '[type f <: AnyKind; f] => 
        //     report.errorAndAbort(s"Ok, we matched: ${Type.show[f]}")
        //   case _ => report.errorAndAbort("No in here ffs")
        // }
        
      case _ =>  report.errorAndAbort("No")
    }

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

    val transformation = Transformation.fromStructure(structure)

    val modifiedTransformation = modifiers.foldLeft(transformation)((acc, mod) => acc.applyModifier(mod))

    Interpreter.runTransformation(tuple, modifiedTransformation)

  }
}
