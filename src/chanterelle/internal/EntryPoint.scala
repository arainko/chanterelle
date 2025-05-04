package chanterelle.internal

import scala.quoted.*

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    val struct = Structure.toplevel[A]
    Logger.info("", struct)
    '{}
  }
}
