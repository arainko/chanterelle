package chanterelle

import chanterelle.interop.Collection

final case class NonEmptyList[+A](head: A, tail: List[A]) {
  def toList: List[A] = head :: tail
}

object NonEmptyList {

  def fromList[A](list: List[A]): Option[NonEmptyList[A]] = list match {
    case head :: tail => Some(NonEmptyList(head, tail))
    case Nil          => None
  }

  given intoIterator[A]: Collection.IntoIterator[A, NonEmptyList[A]] =
    Collection.IntoIterator.from(_.toList.iterator)

  given optionFallibleBuilder[A]: Collection.Builder.Fallible[Option, A, NonEmptyList[A]] =
    Collection.Builder
      .from[List[A]]
      .transform(NonEmptyList.fromList)
      .fallible

  given eitherFallibleBuilder[A]: Collection.Builder.Fallible[[a] =>> Either[String, a], A, NonEmptyList[A]] =
    Collection.Builder
      .from[List[A]]
      .transform(list => NonEmptyList.fromList(list).toRight("empty NonEmptyList"))
      .fallible
}
