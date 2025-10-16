package chanterelle

import munit.FunSuite

import scala.annotation.nowarn
import scala.collection.SortedSet
import scala.collection.immutable.HashMap

class ModifiersSpec extends ChanterelleSuite {
  test(".put puts a new field into a named tuple") {
    val tup = (anotherField = (field1 = 123))
    val expected = (anotherField = (field1 = 123, newField = "garmanbozia"))
    val actual = tup.transform(_.put(_.anotherField)((newField = "garmanbozia")))

    assertEquals(actual, expected)
  }

  test(".remove removes a field") {
    val tup = (anotherField = (field1 = 123, field2 = 123))
    val expected = (anotherField = (field1 = 123))
    val actual = tup.transform(_.remove(_.anotherField.field2))

    assertEquals(actual, expected)
  }

  test(".remove leaves an EmptyTuple when all fields have been deleted") {
    val tup = (anotherField = (field1 = 123))
    val expected = (anotherField = EmptyTuple)
    val actual = tup.transform(_.remove(_.anotherField.field1))

    assertEquals(actual, expected)
  }

  test(".update updates a value under a given path") {
    val tup = (anotherField = (field1 = 123))
    val expected = (anotherField = (field1 = 124))
    val actual = tup.transform(_.update(_.anotherField.field1)(_ + 1))

    assertEquals(actual, expected)
  }

  test(".compute puts a new field into a named tuple") {
    val tup = (anotherField = (field1 = 123))
    val expected = (anotherField = (field1 = 123, newField = 123 + 23))
    val actual = tup.transform(_.compute(_.anotherField)(value => (newField = value.field1 + 23)))

    assertEquals(actual, expected)
  }

  test("modifiers can traverse Options") {
    val tup = (anotherField = Some((field1 = 123)))
    val expected = (anotherField = Some(field1 = 123, newField = "electricityyyy"))
    val actual = tup.transform(_.put(_.anotherField.element)((newField = "electricityyyy")))

    assertEquals(actual, expected)
  }

  test("modifiers can traverse collections (and keep the same collection type)") {
    val tup = (anotherField = List((field1 = 123), (field1 = 124)))
    val expected =
      (anotherField =
        List(
          (field1 = 123, newField = "ashtray wasp"),
          (field1 = 124, newField = "ashtray wasp")
        )
      )
    val actual = tup.transform(_.put(_.anotherField.element)((newField = "ashtray wasp")))

    assertEquals(actual, expected)
  }

  test("modifiers can traverse maps (and keep the same map type)") {
    val tup = (anotherField = HashMap((key = 1) -> (value = 1), (key = 2) -> (value = 2)))
    val expected =
      (anotherField =
        HashMap(
          (key = 1, newField = "the king of limbs is a good album") -> (value = 1, newField = "frfr"),
          (key = 2, newField = "the king of limbs is a good album") -> (value = 2, newField = "frfr")
        )
      )

    val actual =
      tup.transform(
        _.put(_.anotherField.element._1)((newField = "the king of limbs is a good album")),
        _.put(_.anotherField.element._2)((newField = "frfr"))
      )

    assertEquals(actual, expected)
  }

  test("modifiers can traverse sorted collections") {
    val tup = (anotherField = SortedSet(1, 2, 3))
    val expected = (anotherField = SortedSet(2, 3, 4))
    val actual = tup.transform(_.update(_.anotherField.element)(_ + 1))

    assertEquals(actual, expected)
  }

  test("modifiers can traverse classic tuples (using _N accessors)") {
    val tup = (anotherField = (1, 2, (nested = 3)))
    val expected = (anotherField = (1, 2, (nested = 3, newField = "I especially like Separator as the closing track")))

    val actual = tup.transform(_.put(_.anotherField._3)((newField = "I especially like Separator as the closing track")))
    assertEquals(actual, expected)
  }

  test("modifiers can traverse classic tuples (using _.apply(N) accessors)") {
    val tup = (anotherField = (1, 2, (nested = 3)))
    val expected = (anotherField = (1, 2, (nested = 3, newField = "I especially like Separator as the closing track")))

    val actual = tup.transform(_.put(_.anotherField.apply(2))((newField = "I especially like Separator as the closing track")))

    assertEquals(actual, expected)
  }

  test("multiple modifiers in a single transform call behave correctly") {
    val tup = (anotherField = List((field1 = 123, field2 = 0), (field1 = 123, field2 = 0)))

    val actual = tup.transform(
      _.put(_.anotherField.element)((newField1 = 1)),
      _.put(_.anotherField.element)((newField2 = 2)),
      _.put(_.anotherField.element)((newField3 = 3)),
      _.update(_.anotherField.element.field2)(_ + 1),
      _.remove(_.anotherField.element.field1)
    )

    val expected =
      (anotherField =
        List(
          (field2 = 1, newField1 = 1, newField2 = 2, newField3 = 3),
          (field2 = 1, newField1 = 1, newField2 = 2, newField3 = 3)
        )
      )

    assertEquals(actual, expected)
  }

  test("can't modify a subfield of an already modified field") {
    val tup = (anotherField = (field1 = 123))

    assertFailsToCompileContains {
      """
      tup.transform(
        _.update(_.anotherField)(_ => 1),
        _.update(_.anotherField.field1)(_ => "huh"),
      )
      """
    }(
      """Couldn't traverse transformation plan, expected named tuple but encountered configured value"""
    )

  }: @nowarn

  test(".put doesn't accept non-singleton tuples") {
    val tup = (anotherField = (field1 = 123))

    assertFailsToCompileWith {
      """
      tup.transform(_.put(_.anotherField)((newField = "garmanbozia", anotherField = "THE THREAD WILL BE TORN")))
      """
    }(
      """Expected a named tuple with a single field but got: scala.NamedTuple.NamedTuple[scala.Tuple2["newField", "anotherField"], scala.Tuple2[java.lang.String, java.lang.String]]"""
    )

  }: @nowarn

  test(".remove doesn't accept non-field selectors") {
    val tup = (anotherField = (field1 = List(1)))

    assertFailsToCompileWith {
      """
      tup.transform(_.remove(_.anotherField.field1.element))
      """
    }("The selector '_.anotherField.field1.element' isn't pointing to a field of a named tuple")
  }: @nowarn

  test("goofy path selectors are reported as such") {
    val tup = (anotherField = (field1 = List(1)))

    assertFailsToCompileWith {
      """
      tup.transform(_.put(_ => (field = "not really"))((wow = 1)))
      """
    }("""Couldn't parse '_ => (field = "not really")' as a valid path selector""")
  }: @nowarn

  test("big named tuples work") {
    type BigNamedTuple = (
      field1: Int,
      field2: Int,
      field3: Int,
      field4: Int,
      field5: Int,
      field6: Int,
      field7: Int,
      field8: Int,
      field9: Int,
      field10: Int,
      field11: Int,
      field12: Int,
      field13: Int,
      field14: Int,
      field15: Int,
      field16: Int,
      field17: Int,
      field18: Int,
      field19: Int,
      field20: Int,
      field21: Int,
      field22: Int,
      field23: Int
    )

    val input: BigNamedTuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

    val actual = input.transform(
      _.update(_.field1)(_ + 1),
      _.update(_.field12)(_ + 1),
      _.update(_.field23)(_ + 1)
    )

    val expected: BigNamedTuple = (2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24)

    assertEquals(actual, expected)
  }

  test("traversing big tuples works") {
    val input = (field = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))

    val actual =
      input.transform(
        _.update(_.field.apply(0))(_ + 1),
        _.update(_.field.apply(11))(_ + 1),
        _.update(_.field.apply(22))(_ + 1)
      )

    val expected = (field = (2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24))

    assertEquals(actual, expected)
  }

  test("either fields work (left side)") {
    val input = (either = Left((inner = 1)))

    val actual =
      input.transform(
        _.update(_.either.leftElement.inner)(_ + 1)
      )

    val expected = (either = Left((inner = 2)))

    assertEquals(actual, expected)
  }

  test("either fields work (right side)") {
    val input = (either = Right((inner = 1)))

    val actual =
      input.transform(
        _.update(_.either.rightElement.inner)(_ + 1)
      )

    val expected = (either = Right((inner = 2)))

    assertEquals(actual, expected)
  }

  test("renames work") {
    val tup = (
      anotherField = (field1 = 123),
      eitherField = Either.cond("asd".startsWith("a"), (rightField = (field = 1)), (leftField = (field = 1))),
      optField = Some((field = (lowerDown = 1))),
      mapField = Map((key = (k = 1)) -> (value = (v = 1))),
      iterField = Vector((field = (lowerDown = 1)))
    )

    val b: (
      ANOTHERFIELD: (FIELD1: Int, NEWFIELD: Int),
      EITHERFIELD: Either[(LEFTFIELD: (FIELD: Int)), (RIGHTFIELD: (FIELD: Int))],
      OPTFIELD: Option[(FIELD: (LOWERDOWN: Int))],
      MAPFIELD: Map[(KEY: (K: Int)), (VALUE: (V: Int))],
      ITERFIELD: Vector[(FIELD: (LOWERDOWN: Int))]
    ) = tup.transform(
      _.put(_.anotherField)((newField = 3)),
      _.rename(_.toUpperCase)



    )
  }
}

//Block(List(DefDef("$anonfun", List(TermParamClause(List(ValDef("_$64", Inferred(), None)))), Inferred(), Some(Apply(Select(Ident("_$64"), "rename"), List(Block(List(DefDef("$anonfun", List(TermParamClause(List(ValDef("_$65", Inferred(), None)))), Inferred(), Some(Select(Ident("_$65"), "toUpperCase")))), Closure(Ident("$anonfun"), None))))))), Closure(Ident("$anonfun"), None))
