package chanterelle

import scala.collection.generic.IsIterableOnce

opaque type IsCollection[Elem, Collection] = IsIterableOnce[Collection] { type A = Elem }

object IsCollection extends IsCollection.LowPriority {
  private val iterableOnceColl: IsIterableOnce[IterableOnce[Any]] { type A = Any } =
    IsIterableOnce.iterableOnceIsIterableOnce

  extension [Elem, Collection](self: IsCollection[Elem, Collection])
    def iterator(coll: Collection): Iterator[Elem] = self(coll).iterator

  given iterableOnce[Elem, Coll[a] <: IterableOnce[a]]: IsCollection[Elem, Coll[Elem]] =
    iterableOnceColl.asInstanceOf[IsCollection[Elem, Coll[Elem]]]

  private[chanterelle] transparent trait LowPriority { self: IsCollection.type =>
    given fallback[Elem, Collection](using Coll: IsIterableOnce[Collection] { type A = Elem }): IsCollection[Elem, Collection] =
      Coll
  }
}
