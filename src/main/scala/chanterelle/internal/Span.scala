package chanterelle.internal

import scala.quoted.*

private[chanterelle] case class Span(start: Int, end: Int) derives Debug

private[chanterelle] object Span {

  def fromExpr(expr: Expr[Any])(using Quotes): Span = {
    import quotes.reflect.*
    fromPosition(expr.asTerm.pos)
  }

  def fromPosition(using Quotes)(pos: quotes.reflect.Position): Span = Span(pos.start, pos.end)
}