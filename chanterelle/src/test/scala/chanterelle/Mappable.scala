package chanterelle

trait Mappable[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
}
