package chanterelle.interop

import scala.collection.IterableOnceOps
import scala.collection.Factory
import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap as MutHashMap
import scala.collection.MapOps

opaque type CollectionLike[Coll[_], Elem] = IterableOnceOps[Elem, Coll, ?] | CollectionLike.Interopped[Coll, Elem]

object CollectionLike {

  trait Interopped[Coll[_], Elem] {
    def iterator(coll: Coll[Elem]): Iterator[Elem]
    def factory[Elem]: Factory[Elem, Coll[Elem]]
  }

  val hashMap: MapOps[Int, Int, MutHashMap, ?] = MutHashMap(1 -> 1)

  extension [Coll[_], Elem](coll: CollectionLike[Coll, Elem]) {
    def mapped[B](f: Elem => B): Coll[B] = coll match {
      case it: IterableOnceOps[Elem @unchecked, Coll @unchecked, ?] => it.map(f)
      case interop: Interopped[Coll @unchecked, Elem @unchecked]    => interop.iterator(coll).map(f).to(interop.factory[B])
    }
  }
}
