package chanterelle.internal

import scala.quoted.*
import chanterelle.internal.Debug.AST

private[chanterelle] object Erased {
  opaque type K2[F[_, _]] = Expr[F[Any, Any]]

  object K2 {
    given [F[_, _]]: Debug[K2[F]] with {
      def astify(self: K2[F])(using Quotes): AST = Debug.AST.Text("Erased(...)")
    }
  }

  def K2[F[_, _], A, B](expr: Expr[F[A, B]]): K2[F] = expr.asInstanceOf[Expr[F[Any, Any]]]

  extension [F[_, _]: Type](self: K2[F]) {
    def unerase[A: Type, B: Type](using Quotes): Expr[F[A, B]] = self.asExprOf[F[A, B]]
  }

}
