package chanterelle.internal

import scala.quoted.Expr
import chanterelle.Mode
import scala.quoted.Quotes


private[chanterelle] enum Context {
  case Total
  case PossiblyFallible[F[_]](mode: Expr[Mode[F]], wrapperType: WrapperType[F])
}

private[chanterelle] object Context {
  def create(using Quotes): Context = 
    Expr
      .summon[Mode[?]]
      .map {  case '{ type f[_]; $mode: Mode[f] } => Context.PossiblyFallible(mode, WrapperType.create[f])}.getOrElse(Context.Total)
}
