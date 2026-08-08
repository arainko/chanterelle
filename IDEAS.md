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
