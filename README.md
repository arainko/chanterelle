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

```scala
import chanterelle.*
```

which brings in the `.transform` extension method defined on named tuples:

```scala
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


```scala

(toplevelField = (fieldToUpdate = 3, optionalField = Some((anEvenMoreOptionalField = 3, newField = 4))))
```


### Modifiers

* `.put` — adds a new field

```scala
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.put(_.anotherField)((newField = "garmonbozia")))
```


```scala

(anotherField = (field1 = 123, newField = "garmonbozia"))
```


* `.compute` - computes and puts a new field using the selected value
```scala
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.compute(_.anotherField)(value => (newField = value.field1 + 23)))
```


```scala

(anotherField = (field1 = 123, newField = 146))
```


* `.update` - updates a value
```scala
val tup = (anotherField = (field1 = 123))
val transformed = tup.transform(_.update(_.anotherField.field1)(_ + 1))
```


```scala

(anotherField = (field1 = 124))
```


* `.remove` - removes a field of a named or a positional tuple 
```scala
val tup = (anotherField = (field1 = 123, field2 = 123))
val transformed = tup.transform(_.remove(_.anotherField.field2))
```


```scala

(anotherField = (field1 = 123))
```


* `.merge` - deeply merges named tuples

Named tuples are merged by field name. Fields from the named tuple we merge with (the mergee) take precedence. Nested named tuples (that don't come from modifications) and other merged values are recursed. Other values get completely overwritten using the value from the mergee.

```scala
val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
val mergee = (field2 = (level1Field3 = 5, level1Field2 = (anotherField = 6)))

val transformed = tup.transform(_.merge(mergee))
```


```scala

(field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4, anotherField = 6), level1Field3 = 5))
```


Merges can be pointed at a specific nested named tuple with `.regional`.
 
```scala
val tup = (field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4)))
val mergee = (level1Field2 = (anotherField = 6))

val transformed = tup.transform(_.merge(mergee).regional(_.field2))
```


```scala

(field1 = 1, field2 = (level1Field1 = 3, level1Field2 = (level2Field = 4, anotherField = 6)))
```


* `.rename` - transforms the field names
```scala
val tup = (anotherField = (field1 = 123, field2 = 123))

val transformed = tup.transform(_.rename(_.replace("field", "property").toUpperCase))
```


```scala

(ANOTHERFIELD = (PROPERTY1 = 123, PROPERTY2 = 123))
```


The blast radius of the renaming function can be further controlled with '.local' and '.regional':

```scala
val tup = (optField = Some((field = (lowerDown = 1))))

// '.local' renames the toplevel fields
val transformedLocal = tup.transform(_.rename(_.toUpperCase).local(_.optField.element))

// '.regional' makes it so that all the fields underneath the path are transformed
val transformedRegional = tup.transform(_.rename(_.toUpperCase).regional(_.optField.element))
```


```scala

val transformedLocal = (optField = Some((FIELD = (lowerDown = 1))))
val transformedRegional = (optField = Some((FIELD = (LOWERDOWN = 1))))
```

There's also a number of predefined case transformations inside the `FieldName` companion object:
```scala
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


```scala

val camelToSnake = (repo_info = (full_name = "octocat/hello-world", created_at = "2011-01-26T19:01:12Z"))
val camelToKebab = (repo-info = (full-name = "octocat/hello-world", created-at = "2011-01-26T19:01:12Z"))
val snakeToCamel = (repoInfo = (fullName = "octocat/hello-world", createdAt = "2011-01-26T19:01:12Z"))
val kebabToCamel = (repoInfo = (fullName = "octocat/hello-world", createdAt = "2011-01-26T19:01:12Z"))
```

Users can also define their own bundles of transformations by combining various operations on `FieldNames` in a `transparent inline def`:
```scala
transparent inline def renamedAndUppercased(inline fieldName: FieldName) =
   fieldName.rename("someName", "someOtherName").toUpperCase

val tup = (someName = 1)
val transformed = tup.transform(_.rename(renamedAndUppercased))
```


```scala

(SOMEOTHERNAME = 1)
```


### Path selectors

Let's go through a couple of examples of using the path selector:

* accessing and modifying the toplevel value
```scala
val tup = (field1 = 1, field2 = 2)
val transformed = tup.transform(
  _.put(root => root)((newField = 3)) // the selector arg doesn't need to be named 'root', it just needs to be an identity lambda
)
```


```scala

(field1 = 1, field2 = 2, newField = 3)
```


* accessing `Option` or collection elements

The selectors can 'cut through' `Options` and collections alike with the `.element` modifier:
```scala
val tup = (optional = Some(1), coll = Vector(1, 2, 3))
val transformed = tup.transform(
  _.update(_.optional.element)(_ + 1),
  _.update(_.coll.element)(_ + 1)
)
```


```scala

(optional = Some(2), coll = Iterable(2, 3, 4))
```


* accessing keys and values of a `Map`

Much like in the case of collections and `Options`, `Maps` can also be modified with the combination of `.element` and tuple accessors (`._1` for the key and `._2` for the value):
```scala
val tup = (mapField = Map("key1" -> "value1", "key2" -> "value2"))
val transformed = tup.transform(
  _.update(_.mapField.element._1)(_ + "-KEY-UPDATED"),
  _.update(_.mapField.element._2)(_ + "-VALUE-UPDATED")
)
```


```scala

(
  mapField = Map(
    Entry(key = "key1-KEY-UPDATED", value = "value1-VALUE-UPDATED"),
    Entry(key = "key2-KEY-UPDATED", value = "value2-VALUE-UPDATED")
  )
)
```


* accessing both sides of an `Either`

`Either` also gets special treatment via `.leftElement` to access the left side of an `Either` and `.rightElement` to access the right one:
```scala
val tup = (left = Left(1), right = Right("2"))
val transformed = tup.transform(
  _.update(_.left.leftElement)(_ + 1),
  _.update(_.right.rightElement)(_ + "-SUFFIXED")
)
```


```scala

(left = Left(2), right = Right("2-SUFFIXED"))
```

