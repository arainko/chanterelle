# chanterelle

*chanterelle* is a library for seamless named tuple interactions - think deep modifications, field removals, additions and lens-like ops. You know, the works.

## Installation
```scala
libraryDependencies += "io.github.arainko" %% "chanterelle" % "0.1.0"
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
  _.put(_.toplevelField.optionalField.element)((newField = 4)) // the element of an Option or a collection can be accessed with `.element`
)
```
...which, in turn, evaluates to:


```scala

(toplevelField = (fieldToUpdate = 3, optionalField = Some((anEvenMoreOptionalField = 3, newField = 4))))
```


### Modifiers

* `.put` - puts a new field

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
  _.update(_.coll.element)(_ + 1),
)
```


```scala

(optional = Some(2), coll = Vector(2, 3, 4))
```


* accesing keys and values of a `Map`

Much like in the case of collections and `Options`, `Maps` can also be modified with the combination of `.element` and tuple accessors (`._1` for the key and `._2` for the value):
```scala
val tup = (mapField = Map("key1" -> "value1", "key2" -> "value2"))
val transformed = tup.transform(
  _.update(_.mapField.element._1)(_ + "-KEY-UPDATED"),
  _.update(_.mapField.element._2)(_ + "-VALUE-UPDATED"),
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

