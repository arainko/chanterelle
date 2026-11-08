package chanterelle.interop

import chanterelle.Mode

import scala.collection.Factory
import scala.collection.generic.IsIterableOnce
import scala.collection.mutable.Builder as MutBuilder

object Collection {

  /**
   * An interop typeclass representing types that can be converted into iterators.
   * Instances of `Collection.IntoIterator` are usually created with `Collection.IntoIterator.from`, for example:
   *  {{{
   *  import chanterelle.interop.*
   *
   *  given [Elem]: Collection.IntoIterator[Elem, List[Elem]] = Collection.IntoIterator.from(_.iterator)
   *
   *  // or for a Map-shaped collection:
   *  given [Key, Value]: Collection.IntoIterator[(Key, Value), Map[Key, Value]] = Collection.IntoIterator.from(_.iterator)
   *  }}}
   *
   * This typeclass requires its 2 type parameters to be fully-applied and that's because it abstracts over both 'classic' collection shapes (i.e. ones with a single elem type constructor - like List)
   * and Map-shaped ones (i.e. with a two elem type constructor - like HashMap).
   *
   * **Note** that `chanterelle` will only consider `Coll[_]` and `Map[_, _]` shaped instances (like the two aforementioned examples) when looking for candidates during the expansion of its `.transform` macro.
   *
   * An instance of `Collection.IntoIterator` and `Collection.Builder` (or `Collection.Builder.Fallible`) will allow `chanterelle` to consider a given type as a collection type which enables some additional combinators inside the `.transform` macro,
   * like the `.each` operator that 'cuts through' collection shapes when configuring transformations.
   */
  opaque type IntoIterator[Elem, Collection] = IsIterableOnce[Collection] { type A = Elem }

  object IntoIterator extends IntoIterator.LowPriority {

    /**
     * The default way of creating instances of `Collection.IntoIterator`.
     * {{{
     * import chanterelle.interop.*
     * given [Elem]: Collection.IntoIterator[Elem, List[Elem]] = Collection.IntoIterator.from(_.iterator)
     *
     * // or for a Map-shaped collection:
     * given [Key, Value]: Collection.IntoIterator[(Key, Value), Map[Key, Value]] = Collection.IntoIterator.from(_.iterator)
     * }}}
     *
     * The `Coll` type parameter can (usually) be used to infer the type of the `Elem` type param, for example:
     * {{{
     * val intoIter = Collection.IntoIterator.from[List[String]](_.iterator)
     * }}}
     * However, given than `Collection.IntoIterator` is a typeclass it's usually inferred from the its `given` definition (and those always have their type ascription anyway) so users can just drop the type ascriptions altogether.
     */
    def from[Coll](using DummyImplicit)[Elem](toIterator: Coll => Iterator[Elem]): IntoIterator[Elem, Coll] = IntoIter(toIterator)

    extension [Elem, Collection](self: IntoIterator[Elem, Collection])
      def iterator(coll: Collection): Iterator[Elem] = self(coll).iterator

    given iterableOnce[Elem, Coll[a] <: IterableOnce[a]]: IntoIterator[Elem, Coll[Elem]] =
      iterableOnceColl.asInstanceOf[IntoIterator[Elem, Coll[Elem]]]

    private final class IntoIter[Elem, Coll](toIterator: Coll => Iterator[Elem]) extends IsIterableOnce[Coll] {
      type A = Elem

      def apply(coll: Coll): IterableOnce[A] = toIterator(coll)
    }

    private val iterableOnceColl: IsIterableOnce[IterableOnce[Any]] { type A = Any } =
      IsIterableOnce.iterableOnceIsIterableOnce

    private[chanterelle] transparent trait LowPriority { self: IntoIterator.type =>
      given default[Elem, Collection](using Coll: IsIterableOnce[Collection] { type A = Elem }): IntoIterator[Elem, Collection] =
        Coll
    }
  }

  /**
   * An interop typeclass that abstracts over collection builders where the collectiom being built can be empty, its fallible sibling `Collect.Builder.Fallible` allows for
   * constructing collections that are meant to be non-empty (eg. like `cats'` `NonEmptyList`).
   *
   * Custom instances of `Collection.Builder` are usually meant to be built using `Collection.Builder.from` as the starting point and then transforming it into our target type with `.transform`, for example:
   *
   * {{{
   * import chanterelle.interop.*
   *
   * case class MyVector[A](underlying: Vector[A])
   *
   * given [Elem]: Collection.Builder[Elem, MyVector[Elem]] =
   *   Collection.Builder.from[Vector[Elem]].transform(MyVector.apply)
   * }}}
   *
   * ...or if we're operating on a custom Map:
   *
   * {{{
   * import chanterelle.interop.*
   *
   * case class MyMap[K, V](underlying: Map[K, V])
   * given [K, V]: Collection.Builder[(K, V), MyMap[K, V]] = Collection.Builder.from[Map[K, V]].transform(MyMap.apply)
   * }}}
   *
   * An instance of `Collection.Builder` (or `Collection.Builder.Fallible`) and `Collection.IntoIterator` will allow `chanterelle` to consider a given type as a collection type which enables some additional combinators inside the `.transform` macro,
   * like the `.each` operator that 'cuts through' collection shapes when configuring transformations.
   *
   * This typeclass requires its 2 type parameters to be fully-applied and that's because it abstracts over both 'classic' collection shapes (i.e. ones with a single elem type constructor - like List)
   * and Map-shaped ones (i.e. with a two elem type constructor - like HashMap).
   *
   * **Note** that `chanterelle` will only consider `Coll[_]` and `Map[_, _]` shaped instances when looking for candidates during the expansion of its `.transform` macro.
   */
  opaque type Builder[Elem, Collection] <: Builder.Fallible[[a] =>> a, Elem, Collection] =
    Builder.Fallible[[a] =>> a, Elem, Collection]

  object Builder {

    /**
     * The default way of creating instances of `Collection.Builder`. Example:
     * {{{
     * import chanterelle.interop.*
     *
     * case class MyVector[A](underlying: Vector[A])
     *
     * given [Elem]: Collection.Builder[Elem, MyVector[Elem]] =
     *   Collection.Builder.from[Vector[Elem]].transform(MyVector.apply)
     * }}}
     *
     * Note that the `Elem` type param can usually be inferred from just providing the `Collection` type param so this method caters to this use case.
     */
    def from[Collection](using DummyImplicit)[Elem](using factory: Factory[Elem, Collection]): Builder[Elem, Collection] = factory

    /**
     * An alternative to `Collection.Builder.from` where one can feed an explicit `Factory`, for example:
     *
     * {{{
     * import chanterelle.interop.*
     *
     * case class MyVector[A](underlying: Vector[A])
     *
     * given [Elem]: Collection.Builder[Elem, MyVector[Elem]] =
     *   Collection.Builder.fromFactory(Vector).transform(MyVector.apply) // the companion of `Vector` is also a `Factory` of Vectors
     * }}}
     */
    def fromFactory[Elem, Collection](factory: Factory[Elem, Collection]): Builder[Elem, Collection] = factory

    def fromAppendable[Elem, Coll[+elem]](
      empty: Coll[Nothing],
      append: (Coll[Elem], Elem) => Coll[Elem]
    ): Builder[Elem, Coll[Elem]] = new Factory[Elem, Coll[Elem]] {

      override def fromSpecific(it: IterableOnce[Elem]): Coll[Elem] = newBuilder.addAll(it).result()

      override def newBuilder: MutBuilder[Elem, Coll[Elem]] = BuilderFromImmutableAppend(empty, append)
    }

    extension [Elem, Collection](self: Builder[Elem, Collection]) {
      def transform[DestColl](f: Collection => DestColl): Builder[Elem, DestColl] =
        TransformedFactory[[a] =>> a, [a] =>> a, Elem, Collection, DestColl](self, f)

      def fallible[F[_], UnwrappedColl](using ev: Collection <:< F[UnwrappedColl]): Builder.Fallible[F, Elem, UnwrappedColl] =
        self.transform(ev.apply)
    }

    given default[Elem, Collection](using factory: Factory[Elem, Collection]): Builder[Elem, Collection] = factory

    opaque type Fallible[F[_], Elem, Collection] <: Factory[Elem, F[Collection]] = Factory[Elem, F[Collection]]

    object Fallible {
      given fromTotal[F[_], Elem, Collection](using
        F: Mode[F],
        Builder: Collection.Builder[Elem, Collection]
      ): Collection.Builder.Fallible[F, Elem, Collection] =
        Builder.transform(F.pure)

      extension [F[_], Elem, Coll](self: Builder.Fallible[F, Elem, Coll]) {
        def transformK[G[_]](fk: [a] => F[a] => G[a]): Builder.Fallible[G, Elem, Coll] =
          TransformedFactory[F, G, Elem, Coll, Coll](self, fk.apply)
      }

      def fromFactory[F[_], Elem, Collection](factory: Factory[Elem, F[Collection]]): Builder.Fallible[F, Elem, Collection] =
        factory
    }

    private final class TransformedFactory[F[_], G[_], Elem, SourceColl, DestColl](
      factory: Factory[Elem, F[SourceColl]],
      f: F[SourceColl] => G[DestColl]
    ) extends Factory[Elem, G[DestColl]] {

      override def fromSpecific(it: IterableOnce[Elem]): G[DestColl] = f(factory.fromSpecific(it))

      override def newBuilder: MutBuilder[Elem, G[DestColl]] = factory.newBuilder.mapResult(f)
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

  }
}
