package chanterelle.internal

import scala.quoted.Expr
import chanterelle.Mode
import scala.quoted.Quotes
import chanterelle.internal.FallibleInterpreter.TransformationMode
import chanterelle.internal.Debug.AST

private[chanterelle] object Fallible {
  given Debug[Fallible] with {
    def astify(self: Fallible)(using Quotes): AST = AST.Text("Fallible")
  }
}
private[chanterelle] type Fallible = Fallible.type

private[chanterelle] sealed trait Context[+F <: Fallible] {
  def asTotal: Context.Total.type = Context.Total

  inline def locally[A](inline f: Context[F] ?=> A): A = f(using this)
}

private[chanterelle] object Context {
  type Of[FF <: Fallible] = Context[FF]
  type Any = Context[?]

  transparent inline def current(using ctx: Context.Any): ctx.type = ctx

  def create(using Quotes): Context.Any =
    Expr
      .summon[Mode[?]]
      .map {
        case '{ type f[_]; $mode: Mode[f] } => Context.PossiblyFallible(TransformationMode.create(mode), WrapperType.create[f])
      }
      .getOrElse(Context.Total)

  case object Total extends Context[Nothing]
  case class PossiblyFallible[G[_]](mode: TransformationMode[G], wrapperType: WrapperType[G]) extends Context[Fallible]
}
