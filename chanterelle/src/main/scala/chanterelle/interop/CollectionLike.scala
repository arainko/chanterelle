package chanterelle.interop

import scala.collection.IterableOnceOps
import scala.collection.Factory
import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap as MutHashMap
import scala.collection.MapOps
import scala.collection.immutable.SortedSet

object BuiltIn
type BuiltIn = BuiltIn.type

// opaque type IsColl[Coll[_]] = BuiltIn | Support.Collection[Coll]

object IsColl {

  // def iterator[Coll[_]: IsColl as Coll, Elem](coll: Coll[Elem]): Iterator[Elem] = Coll match {
  //   case BuiltIn                        => coll.asInstanceOf[IterableOnceOps[Elem, Coll, ?]].toIterator
  //   case supp: Support.Collection[Coll] => supp.iterator(coll)
  // }

}

object Support {

  trait Collection[Coll[_], Elem] {
    def iterator(coll: Coll[Elem]): Iterator[Elem]
    def factory: Factory[Elem, Coll[Elem]]
  }

  // trait Mode[F[_]] {
  //
  //   def traverseCollection[Coll[_]: Collection as Coll, A, B](source: Coll[A], f: A => F[B]): F[Coll[B]] = {
  //     val iter = Coll.iterator(source)
  //     val builder = Coll.factory[B].newBuilder
  //     while iter.hasNext do {
  //       ???
  //     }
  //
  //     ???
  //   }
  // }

  val hashMap: MapOps[Int, Int, MutHashMap, ?] = MutHashMap(1 -> 1)

  given builtInColl[Elem, Coll[a] <: IterableOnce[a]](using Fac: Factory[Elem, Coll[Elem]]): Collection[Coll, Elem] with {

    def factory: Factory[Elem, Coll[Elem]] = Fac

    def iterator(coll: Coll[Elem]): Iterator[Elem] = coll.iterator
  }

  builtInColl[Int, SortedSet]

  // extension [Coll[_], Elem](coll: CollectionLike[Coll, Elem]) {
  //   def mapped[B](f: Elem => B): Coll[B] = coll match {
  //     case it: IterableOnceOps[Elem @unchecked, Coll @unchecked, ?] => it.map(f)
  //     case interop: Interopped[Coll @unchecked, Elem @unchecked]    => interop.iterator(coll).map(f).to(interop.factory[B])
  //   }
  // }
}
