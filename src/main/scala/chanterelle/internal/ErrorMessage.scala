package chanterelle.internal

import scala.quoted.*

private[chanterelle] sealed trait ErrorMessage derives Debug {
  def span: Span | None = None
}

private[chanterelle] object ErrorMessage {
  case class NoFieldFound(name: String) extends ErrorMessage
  case class NoFieldAtIndexFound(index: Int) extends ErrorMessage
  case class AlreadyConfigured(name: String) extends ErrorMessage
  case class ExpectedSingletonNamedTuple(actual: Type[?], override val span: Span) extends ErrorMessage
  case class SelectorNeedsToPointToAField(path: Path, override val span: Span) extends ErrorMessage
  case class UnexpectedTransformation(expected: String) extends ErrorMessage
  case class NoFactoryFound(tycon: Type[?], elemTpe: Type[?]) extends ErrorMessage
}
