package chanterelle.interop

/**
  * `Mappable` is an interop typeclass that allows `chanterelle` to cut through wrapper types (by allowing `.element` calls inside path selectors inside the `.transform` macro). For example:
  *
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
  * tup.transform(_.update(_.field.element)(_ + 1)) // evaluates to (field = MyWrapper(2)), the instance of `Mappable` allows for the `.element` call to go through
  * }}}
  */
trait Mappable[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
}
