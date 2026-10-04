package chanterelle.internal

import scala.annotation.unused
import scala.quoted.*

private[chanterelle] sealed trait ProductConstructor {
  def apply(fields: Seq[Expr[Any]])(using Quotes): Expr[Any]
}

private[chanterelle] object ProductConstructor {
  final class Primary(tpe: Type[?]) extends ProductConstructor {
    def apply(fields: Seq[Expr[Any]])(using Quotes): Expr[Any] = {
      import quotes.reflect.*

      Typed(Expr.ofTupleFromSeq(fields).asTerm, TypeTree.of(using tpe)).asExpr
    }
  }

  final class Tuple(@unused structure: Structure.Tuple) extends ProductConstructor {
    def apply(fields: Seq[Expr[Any]])(using Quotes): Expr[Any] =
      Expr.ofTupleFromSeq(fields)
  }

}
