# chanterelle

*chanterelle* is a library for seamless named tuple interactions - think deep modifications, field removals, additions and lens-like ops. You know, the works.

## Installation
```scala
libraryDependencies += "io.github.arainko" %% "chanterelle" % "0.1.0"
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
  _.put(_.toplevelField.optionalField.element)((newField = 4)) // the element of an Option or a collection can be accessed with `.element`
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
  _.update(_.coll.element)(_ + 1),
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
  _.update(_.mapField.element._2)(_ + "-VALUE-UPDATED"),
)
```

```scala mdoc:passthrough

Docs.prettyPrint(transformed)
```
