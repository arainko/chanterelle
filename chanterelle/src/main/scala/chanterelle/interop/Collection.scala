package chanterelle.interop

import scala.collection.Factory
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.Builder as MutBuilder

object Collection {

  opaque type IntoIterator[Elem, Collection] = IsIterableOnce[Collection] { type A = Elem }

  object IntoIterator extends IntoIterator.LowPriority {

    def from[Elem, Coll](toIterator: Coll => Iterator[Elem]): IntoIterator[Elem, Coll] = IntoIter(toIterator)

    private final class IntoIter[Elem, Coll](toIterator: Coll => Iterator[Elem]) extends IsIterableOnce[Coll] {
      type A = Elem

      def apply(coll: Coll): IterableOnce[A] = toIterator(coll)
    }

    private val iterableOnceColl: IsIterableOnce[IterableOnce[Any]] { type A = Any } =
      IsIterableOnce.iterableOnceIsIterableOnce

    extension [Elem, Collection](self: IntoIterator[Elem, Collection])
      def iterator(coll: Collection): Iterator[Elem] = self(coll).iterator

    given iterableOnce[Elem, Coll[a] <: IterableOnce[a]]: IntoIterator[Elem, Coll[Elem]] =
      iterableOnceColl.asInstanceOf[IntoIterator[Elem, Coll[Elem]]]

    private[chanterelle] transparent trait LowPriority { self: IntoIterator.type =>
      given default[Elem, Collection](using Coll: IsIterableOnce[Collection] { type A = Elem }): IntoIterator[Elem, Collection] =
        Coll
    }
  }

  opaque type Builder[Elem, Collection] <: Factory[Elem, Collection] = Factory[Elem, Collection]

  object Builder {

    def from[Elem, Collection](factory: Factory[Elem, Collection]): Builder[Elem, Collection] = factory

    // TODO: better name! this sucks
    def fromAppendable[Elem, Coll[+elem]](
      empty: Coll[Nothing],
      append: (Coll[Elem], Elem) => Coll[Elem]
    ): Builder[Elem, Coll[Elem]] = new Factory[Elem, Coll[Elem]] {

      override def fromSpecific(it: IterableOnce[Elem]): Coll[Elem] = newBuilder.addAll(it).result()

      override def newBuilder: MutBuilder[Elem, Coll[Elem]] = BuilderFromImmutableAppend(empty, append)

    }

    extension [Elem, Collection](self: Builder[Elem, Collection]) {
      def transform[DestColl](f: Collection => DestColl): Builder[Elem, DestColl] = TransformedFactory(self, f)
    }

    private final class TransformedFactory[Elem, SourceColl, DestColl](
      factory: Factory[Elem, SourceColl],
      f: SourceColl => DestColl
    ) extends Factory[Elem, DestColl] {

      override def fromSpecific(it: IterableOnce[Elem]): DestColl = f(factory.fromSpecific(it))

      override def newBuilder: MutBuilder[Elem, DestColl] = factory.newBuilder.mapResult(f)
    }

    // inspired from fs2's Collector.Builder impl for Chunk: https://github.com/typelevel/fs2/blob/8aa47aba38454789ce206ad282ed30d45dcbae26/core/shared/src/main/scala/fs2/Chunk.scala#L1437
    // licensed under the MIT license
    private final class BuilderFromImmutableAppend[Elem, Coll[+elem]](
      empty: Coll[Nothing],
      append: (Coll[Elem], Elem) => Coll[Elem]
    ) extends MutBuilder[Elem, Coll[Elem]] {
      private var builder: Coll[Elem] = empty

      override def clear(): Unit = builder = empty

      override def result(): Coll[Elem] = builder

      override def addOne(elem: Elem): this.type = { builder = append(builder, elem); this }

    }

    given default[Elem, Collection](using factory: Factory[Elem, Collection]): Builder[Elem, Collection] = factory
  }
}
