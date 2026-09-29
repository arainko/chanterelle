package chanterelle

import scala.collection.generic.IsIterableOnce
import scala.collection.Factory
import scala.collection.mutable
import scala.collection.mutable.Builder

opaque type IsCollection[Elem, Collection] = IsIterableOnce[Collection] { type A = Elem }

object IsCollection extends IsCollection.LowPriority {

  def make[Elem, Coll[_]](toIterator: Coll[Elem] => Iterator[Elem]): IsCollection[Elem, Coll[Elem]] = IntoIterator(toIterator)

  private final class IntoIterator[Elem, Coll[_]](toIterator: Coll[Elem] => Iterator[Elem]) extends IsIterableOnce[Coll[Elem]] {
    type A = Elem

    def apply(coll: Coll[Elem]): IterableOnce[A] = toIterator(coll)
  }

  private val iterableOnceColl: IsIterableOnce[IterableOnce[Any]] { type A = Any } =
    IsIterableOnce.iterableOnceIsIterableOnce

  extension [Elem, Collection](self: IsCollection[Elem, Collection])
    def iterator(coll: Collection): Iterator[Elem] = self(coll).iterator

  given iterableOnce[Elem, Coll[a] <: IterableOnce[a]]: IsCollection[Elem, Coll[Elem]] =
    iterableOnceColl.asInstanceOf[IsCollection[Elem, Coll[Elem]]]

  private[chanterelle] transparent trait LowPriority { self: IsCollection.type =>
    given default[Elem, Collection](using Coll: IsIterableOnce[Collection] { type A = Elem }): IsCollection[Elem, Collection] =
      Coll
  }
}

opaque type CollectionBuilder[Elem, Collection] = Factory[Elem, Collection]

object CollectionBuilder {

  def fromFactory[Elem, Collection](factory: Factory[Elem, Collection]): CollectionBuilder[Elem, Collection] = factory

  def fromAppendable[Elem, Coll[+elem]](
    empty: Coll[Nothing],
    append: (Coll[Elem], Elem) => Coll[Elem]
  ): CollectionBuilder[Elem, Coll[Elem]] = new Factory[Elem, Coll[Elem]] {

    override def fromSpecific(it: IterableOnce[Elem]): Coll[Elem] = newBuilder.addAll(it).result()

    override def newBuilder: Builder[Elem, Coll[Elem]] = BuilderFromImmutableAppend(empty, append)

  }

  extension [Elem, Collection](self: CollectionBuilder[Elem, Collection]) {
    def transform[DestColl](f: Collection => DestColl): CollectionBuilder[Elem, DestColl] = TransformedFactory(self, f)
  }

  private final class TransformedFactory[Elem, SourceColl, DestColl](
    factory: Factory[Elem, SourceColl],
    f: SourceColl => DestColl
  ) extends Factory[Elem, DestColl] {

    override def fromSpecific(it: IterableOnce[Elem]): DestColl = f(factory.fromSpecific(it))

    override def newBuilder: Builder[Elem, DestColl] = factory.newBuilder.mapResult(f)
  }

  // inspired from fs2's Collector.Builder impl for Chunk: https://github.com/typelevel/fs2/blob/8aa47aba38454789ce206ad282ed30d45dcbae26/core/shared/src/main/scala/fs2/Chunk.scala#L1437
  // licensed under the MIT license
  private final class BuilderFromImmutableAppend[Elem, Coll[+elem]](
    empty: Coll[Nothing],
    append: (Coll[Elem], Elem) => Coll[Elem]
  ) extends Builder[Elem, Coll[Elem]] {
    private var builder: Coll[Elem] = empty

    override def clear(): Unit = builder = empty

    override def result(): Coll[Elem] = builder

    override def addOne(elem: Elem): this.type = { builder = append(builder, elem); this }

  }

  given default[Elem, Collection](using factory: Factory[Elem, Collection]): CollectionBuilder[Elem, Collection] = factory
}
