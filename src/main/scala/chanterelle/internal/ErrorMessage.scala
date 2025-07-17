package chanterelle.internal

import scala.quoted.*

case class Span(start: Int, end: Int) derives Debug

object Span {

  def fromExpr(expr: Expr[Any])(using Quotes): Span = {
    import quotes.reflect.*
    fromPosition(expr.asTerm.pos)
  }

  def fromPosition(using Quotes)(pos: quotes.reflect.Position): Span = Span(pos.start, pos.end)
}

sealed trait ErrorMessage derives Debug {
  def span: Span | None = None
}

object ErrorMessage {
  case class NoFieldFound(name: String) extends ErrorMessage
  case class NoFieldAtIndexFound(index: Int) extends ErrorMessage
  case class AlreadyConfigured(name: String) extends ErrorMessage
  case class ExpectedSingletonNamedTuple(actual: Type[?], override val span: Span) extends ErrorMessage
  case class SelectorNeedsToPointToAField(path: Path, override val span: Span) extends ErrorMessage
}
