package chanterelle.internal

import scala.quoted.Expr
import chanterelle.Mode
import scala.quoted.Quotes

private[chanterelle] object Fallible
private[chanterelle] type Fallible = Fallible.type

private[chanterelle] sealed trait Context {
  type F <: Fallible


  final def reify[FF <: Fallible](value: Transformation[F])(using ev: Transformation[F] =:= Transformation[FF]) =
    ev(value)

}

private[chanterelle] object Context {
  type Of[FF <: Fallible]  = Context { type F = FF }

  transparent inline def current(using ctx: Context): ctx.type = ctx

  def create(using Quotes): Context =
    Expr
      .summon[Mode[?]]
      .map { case '{ type f[_]; $mode: Mode[f] } => Context.PossiblyFallible(mode, WrapperType.create[f]) }
      .getOrElse(Context.Total)

  case object Total extends Context { type F = Nothing }
  case class PossiblyFallible[G[_]](mode: Expr[Mode[G]], wrapperType: WrapperType[G]) extends Context { final type F = Fallible }
}
