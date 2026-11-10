package chanterelle.hidden

import chanterelle.interop.{ Collection, Mappable }

sealed trait Selector {
  extension [A](self: Option[A] | Iterable[A]) @deprecated("use .some for Option and .each for Iterable") def element: A

  extension [E, A](self: Either[E, A]) {
    def leftElement: E
    def rightElement: A
  }

  extension [A](self: Option[A]) {

    /**
     * Cut through an `Option` in the context of a path selector in the `.transform` macro. Example:
     * {{{
     * import chanterelle.*
     *
     * val tup = (field = Some(1))
     * // evaluates to (field = Some(2))
     * tup.transform(_.update(_.field.some)(_ + 1))
     * }}}
     */
    def some: A
  }

  extension [Coll](self: Coll) {

    /**
     * Cut through a collection in the context of a path selector in the `.transform` macro. Example:
     * {{{
     * import chanterelle.*
     *
     * val tup = (field = List(1, 2, 3))
     * // evaluates to (field = List(2, 3, 4))
     * tup.transform(_.update(_.field.each)(_ + 1))
     * }}}
     */
    def each[Elem](using Collection.IntoIterator[Elem, Coll]): Elem
  }

  extension [A](self: A) {

    /**
     * Cut through a 'mappable' type in the context of a path selector in the `.transform` macro.
     * Note that `Mode[F] <: Mappable[F]` so that also works for fallible types.
     *
     * Example:
     * {{{
     * import chanterelle.*
     * import chanterelle.interop.*
     *
     * case class MyWrapper[A](value: A)
     *
     * object MyWrapper {
     *   given Mappable[MyWrapper] with {
     *     def map[A, B](fa: MyWrapper[A], f: A => B): MyWrapper[B] = fa.copy(value = f(fa.value))
     *   }
     * }
     *
     * val tup = (field = MyWrapper(1))
     * // evaluates to (field = MyWrapper(2))
     * tup.transform(_.update(_.field.element)(_ + 1))
     * }}}
     */
    def element[F[_], Elem](using A <:< F[Elem], Mappable[F]): Elem
  }
}
