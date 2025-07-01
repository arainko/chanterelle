package chanterelle.internal

import scala.quoted.*

private[internal] sealed trait PartialMatch[A, Res]

private[internal] object PartialMatch {
  def when[B](using DummyImplicit)[A >: B, C](f: B => C): PartialMatch[A, C] = ???
}
