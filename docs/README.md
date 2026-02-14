# chanterelle

*chanterelle* is a library for seamless named tuple interactions - think deep modifications, field removals, additions and lens-like ops. You know, the works.

## Installation
```scala
libraryDependencies += "io.github.arainko" %% "chanterelle" % "0.1.2"

// or if you're using Scala.js or Scala Native
libraryDependencies += "io.github.arainko" %%% "chanterelle" % "0.1.2"
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

* `.put` - puts a new field

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

* `.merge` - deeply merges named tuples

Named tuples are merged by field name, fields from the named tuple we merge with (the mergee) take precedence, nested named tuples (that don't come from modifications) and other merged values are recursed, other values get completely overwritten using the value from the mergee.

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

// '.regional' makes it so that all the of fields underneath the path are transformed
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
    createdAt = "2011-01-26T19:01:12Z",
  )
)

val snake = (
  repo_info = (
    full_name = "octocat/hello-world",
    created_at = "2011-01-26T19:01:12Z",
  )
)

val kebab = (
  `repo-info` = (
    `full-name` = "octocat/hello-world",
    `created-at` = "2011-01-26T19:01:12Z",
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

Users can also define their own bundles of transformations by combaning various operations on `FieldNames` in a `transparent inline def`:
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

* accesing keys and values of a `Map`

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
