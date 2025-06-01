package chanterelle

import scala.quoted.*

sealed trait Field[A] extends Selectable {

  def selectDynamic(name: String): Nothing = ???
}

object Field {
  sealed trait Of[A] { type Path }

  object Of {
    transparent inline given derived[A]: (Of[A] { type Path }) = ${ derivedMacro[A] }

    private def derivedMacro[A: Type](using Quotes): Expr[Of[A]] = {
      import chanterelle.internal.*

      val struct = Structure.toplevel[A]
      val fieldTpe = FieldDeriver.fromStructure(struct)

      fieldTpe match {
        case '[repr] =>
          '{
            new Of[A] {
              type Path = repr
            }
          }
      }
    }
  }

}
