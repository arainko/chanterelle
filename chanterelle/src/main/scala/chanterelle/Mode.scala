package chanterelle

import scala.collection.Factory
import scala.collection.generic.IsSeq

sealed trait Mode[F[_]] {
  def pure[A](value: A): F[A]

  def map[A, B](fa: F[A], f: A => B): F[B]

  // TODO: reevaluate this - I don't think we even need 'AColl' as a type param, we just need something that can TURN into an Iterable (so having IsIterable in scope?) - BColl also doesn't need to be <: Iterable[B]? As long as there is a Factory in scope
  def traverseCollection[A, B, AColl <: Iterable[A], BColl](
    collection: AColl,
    transformation: A => F[B]
  )(using Factory[B, BColl]): F[BColl]
}

object Mode {
  IsSeq
  trait FailFast[F[_]] extends Mode[F] {
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  }

  trait Accumulating[F[_]] extends Mode[F] {
    def zip[A, B](left: F[A], right: F[B]): F[(A, B)]
  }

  extension [F[_], M <: Mode[F]](self: M) {
    inline def apply[A](inline f: M ?=> A): A = f(using self)
  }

  object Accumulating {
    def either[E, Coll[x] <: Iterable[x]](using Factory[E, Coll[E]]): Mode.Accumulating[[A] =>> scala.Either[Coll[E], A]] =
      Either[E, Coll]

    private final class Either[E, Coll[x] <: Iterable[x]](using errorCollFactory: Factory[E, Coll[E]])
        extends Mode.Accumulating[[A] =>> scala.Either[Coll[E], A]] {

      override def pure[A](value: A): scala.Either[Coll[E], A] = Right(value)

      override def map[A, B](fa: scala.Either[Coll[E], A], f: A => B): scala.Either[Coll[E], B] = fa.map(f)

      override def zip[A, B](fa: scala.Either[Coll[E], A], fb: scala.Either[Coll[E], B]): scala.Either[Coll[E], (A, B)] =
        (fa, fb) match {
          case (Right(a), Right(b))           => Right(a -> b)
          case (Right(_), err @ Left(_))      => err.asInstanceOf[scala.Either[Coll[E], (A, B)]]
          case (err @ Left(_), Right(_))      => err.asInstanceOf[scala.Either[Coll[E], (A, B)]]
          case (Left(errorsA), Left(errorsB)) =>
            val builder = errorCollFactory.newBuilder
            val accumulated = builder ++= errorsA ++= errorsB
            Left(accumulated.result())
        }

      // Inspired by chimney's implementation: https://github.com/scalalandio/chimney/blob/53125c0a55479763157909ef920e11f5b487b182/chimney/src/main/scala/io/scalaland/chimney/TransformerFSupport.scala#L153
      override def traverseCollection[A, B, AColl <: Iterable[A], BColl](
        collection: AColl,
        transformation: A => scala.Either[Coll[E], B]
      )(using
        factory: Factory[B, BColl]
      ): scala.Either[Coll[E], BColl] = {
        val accumulatedErrors = errorCollFactory.newBuilder
        val accumulatedSuccesses = factory.newBuilder
        var isErroredOut = false

        collection.foreach { elem =>
          transformation(elem) match {
            case Left(errors) =>
              accumulatedErrors.addAll(errors)
              if isErroredOut == false then {
                isErroredOut = true
                accumulatedSuccesses.clear()
              }
            case Right(value) =>
              if isErroredOut == false then {
                accumulatedSuccesses.addOne(value)
              }
          }
        }

        if isErroredOut then Left(accumulatedErrors.result()) else Right(accumulatedSuccesses.result())
      }
    }
  }

  object FailFast {
    def either[E]: Mode.FailFast[[A] =>> scala.Either[E, A]] = Either[E]

    private final class Either[E] extends Mode.FailFast[[A] =>> scala.Either[E, A]] {
      final def pure[A](value: A): scala.Either[E, A] = Right(value)

      final def map[A, B](fa: scala.Either[E, A], f: A => B): scala.Either[E, B] = fa.map(f)

      final def flatMap[A, B](fa: scala.Either[E, A], f: A => scala.Either[E, B]): scala.Either[E, B] = fa.flatMap(f)

      final def traverseCollection[A, B, AColl <: Iterable[A], BColl](
        collection: AColl,
        transformation: A => scala.Either[E, B]
      )(using
        factory: Factory[B, BColl]
      ): scala.Either[E, BColl] = {
        var error: Left[E, Nothing] = null
        def isErroredOut = !(error eq null)

        val resultBuilder = factory.newBuilder
        val iterator = collection.iterator
        while iterator.hasNext && !isErroredOut do {
          transformation(iterator.next()) match {
            case err @ Left(_) =>
              error = err.asInstanceOf[Left[E, Nothing]]
              resultBuilder.clear()
            case Right(value) =>
              resultBuilder += value
          }
        }

        if isErroredOut then error else Right(resultBuilder.result())
      }

    }

    def option: Mode.FailFast[scala.Option] = Option

    private object Option extends Mode.FailFast[scala.Option] {

      final def pure[A](value: A): scala.Option[A] = Some(value)

      final def map[A, B](fa: scala.Option[A], f: A => B): scala.Option[B] = fa.map(f)

      final def flatMap[A, B](fa: scala.Option[A], f: A => scala.Option[B]): scala.Option[B] = fa.flatMap(f)

      final def traverseCollection[A, B, AColl <: Iterable[A], BColl](
        collection: AColl,
        transformation: A => scala.Option[B]
      )(using factory: Factory[B, BColl]): scala.Option[BColl] = {
        var isErroredOut = false
        val resultBuilder = factory.newBuilder
        val iterator = collection.iterator
        while iterator.hasNext && !isErroredOut do {
          transformation(iterator.next()) match {
            case None =>
              isErroredOut = true
              resultBuilder.clear()
            case Some(value) =>
              resultBuilder += value
          }
        }

        if isErroredOut then None else Some(resultBuilder.result())
      }

    }
  }
}
