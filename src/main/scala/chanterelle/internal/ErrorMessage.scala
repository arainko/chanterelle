package chanterelle.internal

case class Span(start: Int, end: Int) derives Debug

sealed trait  ErrorMessage derives Debug {
  def span: Span | None = None
}

object ErrorMessage {
  case class NoFieldFound(name: String) extends ErrorMessage
  case class NoFieldAtIndexFound(index: Int) extends ErrorMessage
  case class AlreadyConfigured(name: String) extends ErrorMessage
}
