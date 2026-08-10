package chanterelle.internal

private[chanterelle] opaque type NonEmptyList[+A] = ::[A]

private[chanterelle] object NonEmptyList {
  import scala.collection.immutable.:: as Cons

  private def unsafeCoerce[A](list: List[A]): NonEmptyList[A] = list.asInstanceOf[NonEmptyList[A]]

  private def unsafeCoerceK[F[_], A](wrapped: F[List[A]]): F[NonEmptyList[A]] = wrapped.asInstanceOf[F[NonEmptyList[A]]]

  private[chanterelle] def fromCons[A](cons: ::[A]): NonEmptyList[A] = cons

  private[chanterelle] def apply[A](head: A, tail: A*): NonEmptyList[A] = Cons(head, List(tail*))

  private[chanterelle] def fromList[A](list: List[A]): Option[NonEmptyList[A]] =
    PartialFunction.condOpt(list) { case cons @ (_ :: _) => fromCons(cons) }

  private[chanterelle] given [A: Debug]: Debug[NonEmptyList[A]] = Debug.collection[A, List]

  extension [A](self: NonEmptyList[A]) {
    export toList.{ foldLeft, reduceLeft, head, tail, exists, filter, collect, toVector }

    private[chanterelle] def toList: ::[A] = self

    private[chanterelle] def ::(elem: A): NonEmptyList[A] = Cons(elem, self)

    private[chanterelle] def :::(that: List[A]): NonEmptyList[A] = unsafeCoerce(toList ::: that)

    private[chanterelle] def map[B](f: A => B): NonEmptyList[B] = unsafeCoerce(toList.map(f))

    private[chanterelle] def groupBy[K](f: A => K): Map[K, NonEmptyList[A]] = unsafeCoerceK(self.groupBy(f))

    private[chanterelle] def reverse: NonEmptyList[A] = unsafeCoerce(toList.reverse)
  }
}
