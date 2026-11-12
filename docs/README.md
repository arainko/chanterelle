# chanterelle

*chanterelle* is a library for seamless named tuple interactions - think deep modifications, field removals, additions and lens-like ops. You know, the works.

## Installation
```scala
libraryDependencies += "io.github.arainko" %% "chanterelle" % "0.1.7"

// or if you're using Scala.js or Scala Native
libraryDependencies += "io.github.arainko" %%% "chanterelle" % "0.1.7"
```

## Documentation

The entry point of *chanterelle* is a single import: 

```scala mdoc
import chanterelle.*
```
```scala mdoc:invisible
import chanterelle.internal.*
```

which brings in the `.transform` extension method defined on named tuples:

```scala mdoc:silent
val input = (toplevelField = (nestedField = 1, fieldToUpdate = 2, optionalField = Some((anEvenMoreOptionalField = 3))))

val transformed = input.transform(
  _.update(_.toplevelField.fieldToUpdate)(_ + 1), // note the value of toplevelField.fieldToUpdate in the output
  _.remove(_.toplevelField.nestedField), // toplevelField.nestedField gets removed from the output value
  _.put(_.toplevelField.optionalField.element)(
    (newField = 4)
  ) // the element of an Option or a collection can be accessed with `.element`
)
```
...which, in turn, evaluates to:

```scala mdoc:passthrough
Docs.prettyPrint(transformed)

```

### Modifiers

* `.put` - adds a new field

```scala mdoc:silent:nest
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.put(_.anotherField)((newField = "garmonbozia")))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)

```

* `.compute` - computes and puts a new field using the selected value
```scala mdoc:silent:nest
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.compute(_.anotherField)(value => (newField = value.field1 + 23)))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* `.update` - updates a value
```scala mdoc:silent:nest
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.update(_.anotherField.field1)(_ + 1))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* `.remove` - removes a field of a named or a positional tuple 
```scala mdoc:silent:nest
val tup = (anotherField = (field1 = 123, field2 = 123))
val transformed = tup.transform(_.remove(_.anotherField.field2))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* `.hoist` - 'bubble up' fields wrapped in a wrapper type of your choosing (based on the instance of `Mode` in scope)

```scala mdoc:silent:nest
val tup = (field1 = Some(1), field2 = Some(2), field3 = Some(3))

val transformed = Mode.FailFast.option { tup.transform(_.hoist) }
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

To `hoist` a specific fields you can use the `.hoist` variant that takes a path selector:
```scala mdoc:silent:nest
val tup = (field1 = 1, field2 = (nested = Some(1)))

val transformed = Mode.FailFast.option { tup.transform(_.hoist(_.field2.nested)) }
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

The blast radius of `.hoist` calls can be further controlled with `.local` and `.regional`:

* `.local` hoists the fields a the currently selected named tuple (or a positional tuple) - this is the default behavior of `.hoist`
* `.regional` hoists ALL possible fields in a given region

```scala mdoc:silent:nest
val tup = (field1 = Some(1), field2 = Some(2), field3 = Some(3), nested = (nestedField1 = Some(1)))

// hoists the toplevel fields of the tuple (note that nestedField1 wasn't hoisted), equivalent to '.hoist'
val transformedLocal = Mode.FailFast.option { tup.transform(_.hoist.local) }

// hoists ALL fields in the region (toplevel and nested alike)
val transformedRegional = Mode.FailFast.option { tup.transform(_.hoist.regional) }
```

```scala mdoc:passthrough
Docs.prettyPrintVals(transformedLocal, transformedRegional)
```

There's also a variant of `.local` and `.regional` that takes a path selector that lets the user to select what and where to hoist.

Read more about which `Modes` are available and what can be done with those in #[modes]

* `.merge` - deeply merges named tuples

Named tuples are merged by field name. Fields from the named tuple we merge with (the mergee) take precedence. Nested named tuples (that don't come from modifications) and other merged values are recursed. Other values get completely overwritten using the value from the mergee.

```scala mdoc:silent:nest
val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
val mergee = (field2 = (level1Field3 = 5, level1Field2 = (anotherField = 6)))

val transformed = tup.transform(_.merge(mergee))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

Merges can be pointed at a specific nested named tuple with `.regional`.
 
```scala mdoc:silent:nest
val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
val mergee = (level1Field2 = (anotherField = 6))

val transformed = tup.transform(_.merge(mergee).regional(_.field2))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* `.rename` - transforms the field names
```scala mdoc:silent:nest
val tup = (anotherField = (field1 = 123, field2 = 123))

val transformed = tup.transform(_.rename(_.replace("field", "property").toUpperCase))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

The blast radius of the renaming function can be further controlled with '.local' and '.regional':

```scala mdoc:silent:nest
val tup = (optField = Some((field = (lowerDown = 1))))

// '.local' renames the toplevel fields
val transformedLocal = tup.transform(_.rename(_.toUpperCase).local(_.optField.element))

// '.regional' makes it so that all the fields underneath the path are transformed
val transformedRegional = tup.transform(_.rename(_.toUpperCase).regional(_.optField.element))
```

```scala mdoc:passthrough
Docs.prettyPrintVals(transformedLocal, transformedRegional)
```

There's also a number of predefined case transformations inside the `FieldName` companion object:
```scala mdoc:nest:silent
val camel = (
  repoInfo = (
    fullName = "octocat/hello-world",
    createdAt = "2011-01-26T19:01:12Z"
  )
)

val snake = (
  repo_info = (
    full_name = "octocat/hello-world",
    created_at = "2011-01-26T19:01:12Z"
  )
)

val kebab = (
  `repo-info` = (
    `full-name` = "octocat/hello-world",
    `created-at` = "2011-01-26T19:01:12Z"
  )
)

val camelToSnake = camel.transform(_.rename(FieldName.camelCase.toSnakeCase))
val camelToKebab = camel.transform(_.rename(FieldName.camelCase.toKebabCase))
val snakeToCamel = snake.transform(_.rename(FieldName.snakeCase.toCamelCase))
val kebabToCamel = kebab.transform(_.rename(FieldName.kebabCase.toCamelCase))
```

```scala mdoc:passthrough
Docs.prettyPrintVals(camelToSnake, camelToKebab, snakeToCamel, kebabToCamel)
```

Users can also define their own bundles of transformations by combining various operations on `FieldNames` in a `transparent inline def`:
```scala mdoc:nest:silent
transparent inline def renamedAndUppercased(inline fieldName: FieldName) =
  fieldName.rename("someName", "someOtherName").toUpperCase

val tup = (someName = 1)
val transformed = tup.transform(_.rename(renamedAndUppercased))
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

### Path selectors

Let's go through a couple of examples of using the path selector:

* accessing and modifying the toplevel value
```scala mdoc:silent:nest
val tup = (field1 = 1, field2 = 2)
val transformed = tup.transform(
  _.put(root => root)((newField = 3)) // the selector arg doesn't need to be named 'root', it just needs to be an identity lambda
)
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* accessing `Option` or collection elements

The selectors can 'cut through' `Options` and collections alike with the `.element` modifier:
```scala mdoc:silent:nest
val tup = (optional = Some(1), coll = Vector(1, 2, 3))
val transformed = tup.transform(
  _.update(_.optional.element)(_ + 1),
  _.update(_.coll.element)(_ + 1)
)
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* accessing keys and values of a `Map`

Much like in the case of collections and `Options`, `Maps` can also be modified with the combination of `.element` and tuple accessors (`._1` for the key and `._2` for the value):
```scala mdoc:silent:nest
val tup = (mapField = Map("key1" -> "value1", "key2" -> "value2"))
val transformed = tup.transform(
  _.update(_.mapField.element._1)(_ + "-KEY-UPDATED"),
  _.update(_.mapField.element._2)(_ + "-VALUE-UPDATED")
)
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

* accessing both sides of an `Either`

`Either` also gets special treatment via `.leftElement` to access the left side of an `Either` and `.rightElement` to access the right one:
```scala mdoc:silent:nest
val tup = (left = Left(1), right = Right("2"))
val transformed = tup.transform(
  _.update(_.left.leftElement)(_ + 1),
  _.update(_.right.rightElement)(_ + "-SUFFIXED")
)
```

```scala mdoc:passthrough
Docs.prettyPrint(transformed)
```

### Modes

`Mode` is a capability that provides the ability to 'bubble up' nested wrapped values (i.e. ones wrapped in an `F`) up to the top. `Modes` come in 3 flavors:

* `Accumulating` - the least powerful, able to model error accumulation but the whole structure needs to be known from the get go (in other words, it can't hoist nested fallible fields like `F[F[Int]]`),
* `FailFast` - the intermediate in terms of power - can't model error accumulation, but can hoist nested fallible fields,
* `Parallel` - the most powerful, a mix of both `Accumulating` and `FailFast`. It can model both error accumulation AND hoisting of nested fallible fields.

Note that a `Mode` is not really a typeclass since there isn't really a single instance for each `F` (for example, instances for `Either` could come in all sorts of shapes - sometimes it's best to use `FailFast`, other times it's better to use `Accumulating`). `Modes` are usually provided at the callsite with their `.apply` method that puts them in scope of a block of code.

#### Modes available out of the box

`Mode.Accumulating.either` is an instance of `Mode.Accumulating` for `Either` that collects errors (where the `Errors` type param is the left side of the `Either`). Providing the `Errors` type param is usually enough to infer `E` by itself.

```scala mdoc:silent:nest
val accumulatingOk = Mode.Accumulating.either[List[String]] {
  (field1 = Right(1), field2 = Right(2), field3 = Right(3)).transform(_.hoist)
}

val accumulatingFailed = Mode.Accumulating.either[List[String]] {
  (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
}
```

```scala mdoc:passthrough
Docs.prettyPrintVals(accumulatingOk, accumulatingFailed)
```

`Mode.FailFast.either` is an instance of `Mode.FailFast` for `Either` that fails fast at the first error it encounters (where the `E` type param is the left side of the `Either`):

```scala mdoc:silent:nest
val failFastOk = Mode.FailFast.either[String] {
  (field1 = Right(1), field2 = Right(2), field3 = Right(3)).transform(_.hoist)
}

val failFastFailed = Mode.FailFast.either[String] {
  (field1 = Right(1), field2 = Left("err1"), field3 = Left("err2")).transform(_.hoist)
}
```

```scala mdoc:passthrough
Docs.prettyPrintVals(failFastOk, failFastFailed)
```

`Mode.FailFast.option` fails fast with a `None` once it encounters one:

```scala mdoc:silent:nest
val failFastOptionFailed = Mode.FailFast.option {
  (field1 = Some(1), field2 = None, field3 = Some(3)).transform(_.hoist)
}
```

```scala mdoc:passthrough
Docs.prettyPrint(failFastOptionFailed)
```

`Mode.Parallel.either` is an instance of `Mode.Parallel` for `Either` that collects errors just like `Mode.Accumulating`, but it can also hoist nested fallible fields:

```scala mdoc:silent:nest
val parallelFailed = Mode.Parallel.either[List[String]] {
  (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
}

// hoisting nested fallible fields also works with Mode.Parallel
val parallelNested = Mode.Parallel.either[List[String]] {
  (field1 = Right(Right(1))).transform(_.hoist(_.field1.element))
}
```

```scala mdoc:passthrough
Docs.prettyPrintVals(parallelFailed, parallelNested)
```

### Interop

*chanterelle* can 'cut through' custom wrapper and collection types in path selectors as long as instances of the interop typeclasses are provided.

#### `Mappable`

`Mappable` is an interop typeclass that allows *chanterelle* to cut through wrapper types (by allowing `.element` calls inside path selectors inside the `.transform` macro):

```scala mdoc:silent:nest
import chanterelle.interop.*

case class MyWrapper[A](value: A)

object MyWrapper {
  given Mappable[MyWrapper] with {
    def map[A, B](fa: MyWrapper[A], f: A => B): MyWrapper[B] = fa.copy(value = f(fa.value))
  }
}

val tup = (field = MyWrapper((inner = 1)))

val transformed = tup.transform(_.update(_.field.element.inner)(_ + 1)) 
```

```scala mdoc:passthrough
given showMyWrapper: Show[MyWrapper[(inner: Int)]] = Show.derived[MyWrapper[(inner: Int)]]
Docs.prettyPrint(transformed)
```

#### `Collection.IntoIterator` and `Collection.Builder`

`Collection.IntoIterator` represents types that can be converted into iterators and `Collection.Builder` abstracts over collection builders. Once these instances are provided, *chanterelle* can consider a given type a collection type which unlocks the collection combinators (like `.each`) inside the `.transform` macro:

```scala mdoc:silent:nest
case class MyVector[A](underlying: Vector[A])

object MyVector {
  given [A]: Collection.IntoIterator[A, MyVector[A]] =
    Collection.IntoIterator.from(_.underlying.iterator)

  given [A]: Collection.Builder[A, MyVector[A]] =
    Collection.Builder
      .from[Vector[A]]
      .transform(MyVector.apply)
}

val tup = (field = MyVector(Vector(Right(1), Right(2))))

val ok: Either[String, (field: MyVector[Int])] = Mode.FailFast.either[String] { tup.transform(_.hoist(_.field.each)) }
val failed: Either[String, (field: MyVector[Int])] = Mode.FailFast.either[String] {
  (field = MyVector(Vector(Right(1), Left("boom")))).transform(_.hoist(_.field.each))
}
```

```scala mdoc:passthrough
given showMyVector: Show[MyVector[Int]] = Show.derived[MyVector[Int]]
Docs.prettyPrintVals(ok, failed)
```

#### `Collection.Builder.Fallible`

`Collection.Builder.Fallible` abstracts over collection builders where the construction of a collection can be fallible (for example when the collection being built cannot be empty). Custom instances are usually built by starting with `Collection.Builder.from` as a base, then transforming it with `.transform` into a custom type and finishing up with `.fallible`:

```scala mdoc:silent:nest
case class NonEmptyList[A] private (head: A, tail: List[A])

object NonEmptyList {
  extension [A](value: NonEmptyList[A]) def toList: List[A] = value.head :: value.tail

  def from[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  def fromList[A](list: List[A]): Option[NonEmptyList[A]] = list match {
    case head :: tail => Some(NonEmptyList(head, tail))
    case Nil          => None
  }

  given [A]: Collection.IntoIterator[A, NonEmptyList[A]] =
    Collection.IntoIterator.from(_.toList.iterator)

  given [A]: Collection.Builder.Fallible[Option, A, NonEmptyList[A]] =
    Collection.Builder
      .from[List[A]]
      .transform(NonEmptyList.fromList)
      .fallible[Option] // this type ascription can usually be inferred
}

val tup = (field = NonEmptyList.from(Some(1), Some(2)))

val transformed = Mode.FailFast.option { tup.transform(_.hoist(_.field.each)) }
```

```scala mdoc:passthrough
given showNel: Show[NonEmptyList[Int]] = Show.derived[NonEmptyList[Int]]
Docs.prettyPrint(transformed)
```
