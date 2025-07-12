package chanterelle.internal

case class Span(start: Int, end: Int) derives Debug

sealed trait  ErrorMessage derives Debug {
  def span: Span
}

object ErrorMessage {
  case class NoFieldFound(name: String, span: Span) extends ErrorMessage
}
