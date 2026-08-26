package chanterelle.hidden

import chanterelle.Mode

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [F[_], A](using Mode[F])(self: F[A]) def element: A
}
