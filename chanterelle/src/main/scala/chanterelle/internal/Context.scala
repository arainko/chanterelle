package chanterelle.internal

import scala.quoted.Expr
import chanterelle.Mode
import scala.quoted.Quotes

private[chanterelle] object Fallible
private[chanterelle] type Fallible = Fallible.type

private[chanterelle] sealed trait Context[+F <: Fallible] {

  // final def reify[FF <: Fallible](using DummyImplicit)[G[+x <: Fallible]](value: G[F])(using ev: G[F] <:< G[FF]) =
  // ev(value)

}

private[chanterelle] object Context {
  type Of[FF <: Fallible] = Context[FF]
  type Any = Context[?]
  transparent inline def current(using ctx: Context[?]): ctx.type = ctx

  def create(using Quotes): Context[?] =
    Expr
      .summon[Mode[?]]
      .map { case '{ type f[_]; $mode: Mode[f] } => Context.PossiblyFallible(mode, WrapperType.create[f]) }
      .getOrElse(Context.Total)

  case object Total extends Context[Nothing]
  case class PossiblyFallible[G[_]](mode: Expr[Mode[G]], wrapperType: WrapperType[G]) extends Context[Fallible]
}
