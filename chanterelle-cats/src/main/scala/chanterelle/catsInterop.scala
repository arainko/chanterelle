package chanterelle

import cats.Parallel
import scala.collection.Factory
import cats.syntax.all.*
import cats.data.Chain
import cats.Applicative
import cats.Monad
import scala.collection.generic.IsIterable
import scala.collection.IterableOps
import scala.collection.mutable
import scala.collection.IterableFactory
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.Builder

extension (self: Mode.type) {
  def parallel[F[_]: Parallel]: Mode.Accumulating[F] & Mode.FailFast[F] = ParallelMode[F]
  def applicative[F[_]: Applicative]: Mode.Accumulating[F] = ApplicativeMode[F]
  def monad[F[_]: Monad]: Mode.FailFast[F] = MonadMode[F]
}

private final class ParallelMode[F[_]: Parallel as F] extends Mode.Accumulating[F], Mode.FailFast[F] {
  override def flatMap[A, B](fa: F[A], f: A => F[B]): F[B] = F.monad.flatMap(fa)(f)

  override def zip[A, B](left: F[A], right: F[B]): F[(A, B)] = Parallel.parProduct(left, right)

  override def pure[A](value: A): F[A] =
    F.monad.pure(value)

  override def map[A, B](fa: F[A], f: A => B): F[B] = F.monad.map(fa)(f)

  override def traverseCollection[A, B, BColl](collection: Iterable[A], transformation: A => F[B])(using
    BColl: Factory[B, BColl]
  ): F[BColl] =
    map(collection.toVector.parTraverse(transformation), _.to(BColl))
}

private final class ApplicativeMode[F[_]: Applicative as F] extends Mode.Accumulating[F] {

  override def zip[A, B](left: F[A], right: F[B]): F[(A, B)] = F.product(left, right)

  override def pure[A](value: A): F[A] = F.pure(value)

  override def map[A, B](fa: F[A], f: A => B): F[B] = F.map(fa)(f)

  override def traverseCollection[A, B, BColl](collection: Iterable[A], transformation: A => F[B])(using
    BColl: Factory[B, BColl]
  ): F[BColl] =
    map(collection.toVector.traverse(transformation), _.to(BColl))
}

private final class MonadMode[F[_]: Monad as F] extends Mode.FailFast[F] {

  override def flatMap[A, B](fa: F[A], f: A => F[B]): F[B] = F.flatMap(fa)(f)

  override def pure[A](value: A): F[A] = F.pure(value)

  override def map[A, B](fa: F[A], f: A => B): F[B] = F.map(fa)(f)

  override def traverseCollection[A, B, BColl](collection: Iterable[A], transformation: A => F[B])(using
    BColl: Factory[B, BColl]
  ): F[BColl] =
    map(collection.toVector.traverse(transformation), _.to(BColl))

}
