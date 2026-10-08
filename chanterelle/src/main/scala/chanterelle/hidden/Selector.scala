package chanterelle.hidden

import chanterelle.interop.{ Collection, Mappable }

import scala.annotation.{ compileTimeOnly, unused }
import chanterelle.Mode

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) @deprecated("use .some for Option and .each for Iterable") def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [A](self: Option[A]) def some: A

  extension [Elem, Coll](using Collection.IntoIterator[Elem, Coll])(self: Coll) def each: Elem

  extension [F[_], A](using Mappable[F])(self: F[A]) def element: A
}
