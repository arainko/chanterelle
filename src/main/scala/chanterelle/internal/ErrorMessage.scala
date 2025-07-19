package chanterelle.internal

import scala.quoted.*

private[chanterelle] sealed trait ErrorMessage derives Debug {
  def span: Span | None = None

  def render(using Quotes): String
}

private[chanterelle] object ErrorMessage {
  case class NoFieldFound(name: String) extends ErrorMessage {
    def render(using Quotes) = s"No field '$name' found"
  }

  case class NoFieldAtIndexFound(index: Int) extends ErrorMessage {
    def render(using Quotes) = s"No field at index $index"
    
  }

  case class AlreadyConfigured(name: String) extends ErrorMessage {
    def render(using Quotes) = s"The field '$name' has already been configured"

  }

  case class ExpectedSingletonNamedTuple(actual: Type[?], override val span: Span) extends ErrorMessage {
    def render(using Quotes) = {
      import quotes.reflect.*
      s"Expected a named tuple with a single field but got: ${actual.repr.show(using Printer.TypeReprCode)}"
    }

  }

  case class SelectorNeedsToPointToAField(path: Path, override val span: Span) extends ErrorMessage {
    def render(using Quotes) = s"The selector '${path.render}' isn't pointing to a field of a named tuple"

  }

  case class UnexpectedTransformation(expected: String) extends ErrorMessage {
    def render(using Quotes) = s"Couldn't traverse transformation plan, expected $expected"

  }

  case class NoFactoryFound(tpe: Type[?]) extends ErrorMessage {
    def render(using Quotes) = {
      
      import quotes.reflect.*
      s"Couldn't find an implicit instance of Factory for ${tpe.repr.show(using Printer.TypeReprCode)}"
    }

  }
}
