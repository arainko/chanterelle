package chanterelle.hidden

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }
}
