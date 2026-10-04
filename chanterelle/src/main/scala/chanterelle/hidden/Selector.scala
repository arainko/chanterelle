package chanterelle.hidden

import chanterelle.interop.{ Collection, Mappable }

import scala.annotation.{
  compileTimeOnly,
  unused
}

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [Self, A](using extractor: Selector.Extractor[Self] { type Elem = A })(self: Self) {
    def element: A
  }
}

object Selector {
  sealed trait Extractor[Self] {
    type Elem
  }

  @compileTimeOnly("only usable inside the .transform DSL")
  given mappable[F[_], A](using @unused F: Mappable[F]): Extractor[F[A]] with {
    type Elem = A
  }

  @compileTimeOnly("only usable inside the .transform DSL")
  given collection[A, Coll](using Collection.IntoIterator[A, Coll]): Extractor[Coll] with {
    type Elem = A
  }
}
