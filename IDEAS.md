For fallible transformations:

Idea 1):
declare:

```scala

package chanterelle.hidden 

object Tuples {
...

  object Extensions {
// image these are all extension methods
    def mapEach(...)
    def traverseEach(using Mode[F])(...)
    def sequence ?
    
  }
}

```

then change the type of TupleModifiers that accept a function (like update or compute) to

```scala
Extensions.type ?=> Selected => Value
```

and then somehow introduce a type param on TupleModifier? so that users coudl get `.?(using Mode[F])` based on some shit

Idea 2):

encode each method I want to support (maybe except for mapEach? but then that makes it an outlier) as a TupleModifier
so we get new defs in TupleModifier:

```scala
def traverseEach(using m: Mode[F], Tup: Union[Tup])(f: Tup.Result => F[B])
def mapEach(Tup: Union[Tup])(f: Tup.Result => B)
def sequence(using Mode[F]) // make this only work on tuples with fields wrapped in F[_]
def ?(using Mode[F]) // <-- this one 'hoists' stuff to the top like in Rust
```

What happens for Wrapped(wrapped: Transformation[Fallible], IsHoisted.No)?

Let's say we know that wrapped is an actual fallible transformation that has a hoisted thing inside of it somewhere, what happens?

example:

```scala
val tup = (int = Right((nest1 = Right(3).?)))
val res: Either[Err, (int: Either[Err, (nest1: Int)])] = 
    int.flatMap(_.nest).flatMap(nest => (int = tup.int.map((nest1 = nest))))
```

^ feels kinda dumb doesn't it? Does that mean we need to mark ALL wrappeds on a path (_.int.nest) as hoisted?
I think so - there's no point in doing the opposite because we'd need to .flatMap a bunch of shit to traverse the path anyway.

### What do we do about Optional, EitherLike and IterLike?

```scala
val tup = (int = Some(Right(1)))
```

^ forms an Optional(Wrapped(Leaf[Int]))

When the wrapped node IS NOT hoisted:

```scala
(int = tup.int.map(_.map(int => <transform int>)))
```

```scala
val i = tup.int match {
  case Some(right) => right.map(Some.apply)
  case None => Right(None)
}

i.map(int => (int = int))
```
