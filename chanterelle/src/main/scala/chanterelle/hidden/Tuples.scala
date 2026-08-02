package chanterelle.hidden

import NamedTuple.*

object Tuples {
  def valuesOf[N <: scala.Tuple, V <: scala.Tuple](tup: NamedTuple[N, V]): V = tup.toTuple

  type Mapped[A, B] = [In] =>> In match { case A => B }

  inline def Mapped[A, B](f: A => B): [in] => in => Mapped[A, B][in] =
    [in] => (in) => in match { case a: A => f(a) }

}
