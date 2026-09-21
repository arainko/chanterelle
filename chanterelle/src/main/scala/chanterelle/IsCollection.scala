package chanterelle

import chanterelle.internal.Structure.Collection
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.SortedSet

opaque type IsCollection[Elem, Collection] = IsIterableOnce[Collection] { type A = Elem }

object IsCollection extends IsCollectionLowPriority {
  private val iterableOnceColl: IsIterableOnce[IterableOnce[Any]] { type A = Any } =
    IsIterableOnce.iterableOnceIsIterableOnce

  extension [Elem, Collection](self: IsCollection[Elem, Collection])
    def iterator(coll: Collection): Iterator[Elem] = self(coll).iterator

  given iterableOnce[Elem, Coll[a] <: IterableOnce[a]]: IsCollection[Elem, Coll[Elem]] =
    iterableOnceColl.asInstanceOf[IsCollection[Elem, Coll[Elem]]]

  private[chanterelle] transparent trait IsCollectionLowPriority { self: IsCollection.type =>
    given fallback[Elem, Collection](using Coll: IsIterableOnce[Collection] { type A = Elem }): IsCollection[Elem, Collection] =
      Coll
  }
}
