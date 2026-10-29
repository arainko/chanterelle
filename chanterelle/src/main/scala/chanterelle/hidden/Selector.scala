package chanterelle.hidden

import chanterelle.interop.{ Collection, Mappable }

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) @deprecated("use .some for Option and .each for Iterable") def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [A](self: Option[A]) def some: A

  extension [Coll](self: Coll) def each[Elem](using Collection.IntoIterator[Elem, Coll]): Elem

  extension [A](self: A) def element[F[_], Elem](using A <:< F[Elem], Mappable[F]): Elem
}
