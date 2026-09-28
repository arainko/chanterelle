package chanterelle.hidden

import chanterelle.Mode
import chanterelle.Mappable
import chanterelle.IsCollection

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [F[_], A](using Mappable[F] | IsCollection[A, F[A]])(self: F[A]) def element: A
}
