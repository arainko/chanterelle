package chanterelle

sealed trait Mode[F[_]] {
  def pure[A](value: A): F[A]
  def map[A, B](fa: F[A], f: A => B): F[B]
}

object Mode {
  trait FailFast[F[_]] extends Mode[F] {
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  }

  trait Accumulating[F[_]] extends Mode[F] {
    def zip[A, B](left: F[A], right: F[B]): F[(A, B)]
  }

  extension [F[_], M <: Mode[F]](self: M) {
    inline def locally[A](inline f: M ?=> A): A = f(using self)
  }
}
