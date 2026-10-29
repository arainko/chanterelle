package chanterelle

import chanterelle.interop.{ Collection, Mappable }

import scala.collection.immutable.HashMap

object OpaqueFixtures {

  opaque type OpqVec[+A] = Vector[A]

  object OpqVec {
    def apply[A](vec: Vector[A]): OpqVec[A] = vec

    extension [A](vec: OpqVec[A]) def underlying: Vector[A] = vec

    given intoIter[A]: Collection.IntoIterator[A, OpqVec[A]] = Collection.IntoIterator.from(_.underlying.iterator)

    given builder[A]: Collection.Builder[A, OpqVec[A]] = Collection.Builder
      .fromFactory(Vector)
      .transform(OpqVec.apply)
  }

  opaque type OpqMap[K, +V] = HashMap[K, V]

  object OpqMap {
    def apply[K, V](map: HashMap[K, V]): OpqMap[K, V] = map

    extension [K, V](map: OpqMap[K, V]) def underlying: HashMap[K, V] = map

    given [K, V]: Collection.IntoIterator[(K, V), OpqMap[K, V]] = Collection.IntoIterator.from(_.underlying.iterator)

    given [K, V]: Collection.Builder[(K, V), OpqMap[K, V]] = Collection.Builder
      .fromFactory(HashMap)
      .transform(OpqMap.apply)
  }

  opaque type OpqBox[+A] = (A, String)

  object OpqBox {
    def apply[A](value: A, tag: String): OpqBox[A] = (value, tag)

    given Mappable[OpqBox] with {
      def map[A, B](fa: OpqBox[A], f: A => B): OpqBox[B] = (f(fa._1), fa._2)
    }
  }
}
