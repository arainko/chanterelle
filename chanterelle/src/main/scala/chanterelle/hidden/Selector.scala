package chanterelle.hidden

import chanterelle.Mode
import chanterelle.Mappable

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [F[_], A](using Mappable[F])(self: F[A]) def element: A
}
