### Fallible transformations

* internals - Hoisted, Wrapped, Mapped
  * implementation [X]
  * decide on how to handle iterables in `Mode[F]`. Options:
    * require <: `Iterable[A]` <-- this is what is currently being done, but IMO the bound on the method should be lessened to just `Iterable[A]` not a subtype of this,
    * use a typeclass like `IsIterableOnce` - this theoretically allows us to be extensible but still stuff like Chain from cats doesn't provide a factory or an IsIterable instace :c, implementing IsIterableOnce is very trivial and it provides `.iterator` which looks like a perfect extension point? The `Factory` still feels off (tho implementing a factory for Chain seems trivial if you go through Vector too...):

```scala
private final class ChainIsIterable[Elem] extends IsIterableOnce[Chain[Elem]] {

  type A = Elem

  override def apply(coll: Chain[Elem]): IterableOnce[A] = coll.toIterable

  val asd = apply(??? : Chain[Elem]).iterator

}

private final class ChainFactory[Elem] extends Factory[Elem, Chain[Elem]] {
  override def fromSpecific(it: IterableOnce[Elem]): Chain[Elem] = Chain.fromIterableOnce(it)

  override def newBuilder: Builder[Elem, Chain[Elem]] = Vector.newBuilder[Elem].mapResult(fromSpecific)
}
```

this feels like it'd be immediately better vs what is in ducktape (extensible, at the very least).

    * don't use an Iterable, but require an `Iterator`? this would also require a typeclass of some sorts so maybe that's just dumb

* good errors []
* tests []

* user-visible DSL
  * still need to think it through
