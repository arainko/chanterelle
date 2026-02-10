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

    val actual = tup.transform(
      _.put(_.anotherField)((newField = 3)),
      _.rename(_.toUpperCase)
    )

    val expected = (
      ANOTHERFIELD = (FIELD1 = 123, NEWFIELD = 3),
      EITHERFIELD = Either.cond("asd".startsWith("a"), (RIGHTFIELD = (FIELD = 1)), (LEFTFIELD = (FIELD = 1))),
      OPTFIELD = Some((FIELD = (LOWERDOWN = 1))),
      MAPFIELD = Map((KEY = (K = 1)) -> (VALUE = (V = 1))),
      ITERFIELD = Vector((FIELD = (LOWERDOWN = 1)))
    )

    assertEquals(actual, expected)
  }

  test("local renames work when pointing to a named tuple") {
    val tup = (
      anotherField = (field1 = 123),
      eitherField = Either.cond("asd".startsWith("a"), (rightField = (field = 1)), (leftField = (field = 1))),
      optField = Some((field = (lowerDown = 1))),
      mapField = Map((key = (k = 1)) -> (value = (v = 1))),
      iterField = Vector((field = (lowerDown = 1)))
    )

    val actual = tup.transform(
      _.put(_.anotherField)((newField = 3)),
      _.rename(_.toUpperCase).local(_.optField.element)
    )

    val expected = (
      anotherField = (field1 = 123, newField = 3),
      eitherField = Either.cond("asd".startsWith("a"), (rightField = (field = 1)), (leftField = (field = 1))),
      optField = Some((FIELD = (lowerDown = 1))),
      mapField = Map((key = (k = 1)) -> (value = (v = 1))),
      iterField = Vector((field = (lowerDown = 1)))
    )

    assertEquals(actual, expected)
  }

  test("regional renames work") {
    val tup = (
      anotherField = (field1 = 123),
      eitherField = Either.cond("asd".startsWith("a"), (rightField = (field = 1)), (leftField = (field = 1))),
      optField = Some((field = (lowerDown = 1))),
      mapField = Map((key = (k = 1)) -> (value = (v = 1))),
      iterField = Vector((field = (lowerDown = 1)))
    )

    val actual = tup.transform(
      _.put(_.anotherField)((newField = 3)),
      _.rename(_.toUpperCase).regional(_.eitherField)
    )

    val expected = (
      anotherField = (field1 = 123, newField = 3),
      eitherField = Either.cond("asd".startsWith("a"), (RIGHTFIELD = (FIELD = 1)), (LEFTFIELD = (FIELD = 1))),
      optField = Some((field = (lowerDown = 1))),
      mapField = Map((key = (k = 1)) -> (value = (v = 1))),
      iterField = Vector((field = (lowerDown = 1)))
    )

    assertEquals(actual, expected)
  }

  test("camelCase to snake_case transformation") {
    val camel = (
      simpleField = 1,
      fieldWithNumber1 = 2,
      field2WithNumber = 3,
      _leadingUnderscore = 4,
      trailingUnderscore_ = 5,
      mixedCASEField = 6,
      fieldWith123Numbers = 7,
      fieldWithMultiple___Underscores = 8
    )
    val expectedSnake = (
      simple_field = 1,
      field_with_number1 = 2,
      field2_with_number = 3,
      _leading_underscore = 4,
      trailing_underscore_ = 5,
      mixed_case_field = 6,
      field_with123_numbers = 7,
      field_with_multiple___underscores = 8
    )
    val snake = camel.transform(_.rename(FieldName.camelCase.toSnakeCase))
    assertEquals(snake, expectedSnake)
  }

  test("snake_case to camelCase transformation") {
    val snake = (
      simple_field = 1,
      field_with_number1 = 2,
      field2_with_number = 3,
      _leading_underscore = 4,
      trailing_underscore_ = 5,
      mixed_case_field = 6,
      field_with123_numbers = 7,
      field_with_multiple___underscores = 8
    )
    val expectedCamel = (
      simpleField = 1,
      fieldWithNumber1 = 2,
      field2WithNumber = 3,
      leadingUnderscore = 4,
      trailingUnderscore_ = 5,
      mixedCaseField = 6,
      fieldWith123Numbers = 7,
      fieldWithMultiple__Underscores = 8
    )
    val camel = snake.transform(_.rename(FieldName.snakeCase.toCamelCase))
    assertEquals(camel, expectedCamel)
  }

  test("camelCase to kebab-case transformation") {
    val camel = (
      simpleField = 1,
      fieldWithNumber1 = 2,
      field2WithNumber = 3,
      `-leadingDash` = 4,
      `trailingDash-` = 5,
      mixedCASEField = 6,
      fieldWith123Numbers = 7,
      `fieldWithMultiple---Dashes` = 8,
      fieldWithDash1 = 9
    )
    val expectedKebab = (
      `simple-field` = 1,
      `field-with-number1` = 2,
      `field2-with-number` = 3,
      `-leading-dash` = 4,
      `trailing-dash-` = 5,
      `mixed-case-field` = 6,
      `field-with123-numbers` = 7,
      `field-with-multiple---dashes` = 8,
      `field-with-dash1` = 9
    )
    val kebab = camel.transform(_.rename(FieldName.camelCase.toKebabCase))
    assertEquals(kebab, expectedKebab)
  }

  test("kebab-case to camelCase transformation") {
    val kebab = (
      `simple-field` = 1,
      `field-With-number1` = 2,
      `field2-with-number` = 3,
      `-leading-dash` = 4,
      `trailing-dash-` = 5,
      `mixed-case-field` = 6,
      `field-with123-numbers` = 7,
      `field-with-multiple---dashes` = 8,
      `field-with-dash1` = 9,
      `-Uppercase-leading-dash` = 10
    )
    val expectedCamel = (
      simpleField = 1,
      fieldWithNumber1 = 2,
      field2WithNumber = 3,
      leadingDash = 4,
      `trailingDash-` = 5,
      mixedCaseField = 6,
      fieldWith123Numbers = 7,
      `fieldWithMultiple--Dashes` = 8,
      fieldWithDash1 = 9,
      uppercaseLeadingDash = 10
    )

    val camel = kebab.transform(_.rename(FieldName.kebabCase.toCamelCase))
    assertEquals(camel, expectedCamel)
  }

  test("example API field name transformations with nested fields") {
    val camel = (
      repoInfo = (
        fullName = "octocat/hello-world",
        createdAt = "2011-01-26T19:01:12Z",
        owner = (
          profileImageUrl = "http://a0.twimg.com/profile_images/1135218950/twitterapi_normal.png",
          userName = "octocat"
        )
      ),
      weather = (
        feelsLike = 278.4,
        tempMin = 279.15,
        tempMax = 281.15,
        details = (
          humidityLevel = 81,
          pressureValue = 1012
        )
      )
    )

    val expectedSnake = (
      repo_info = (
        full_name = "octocat/hello-world",
        created_at = "2011-01-26T19:01:12Z",
        owner = (
          profile_image_url = "http://a0.twimg.com/profile_images/1135218950/twitterapi_normal.png",
          user_name = "octocat"
        )
      ),
      weather = (
        feels_like = 278.4,
        temp_min = 279.15,
        temp_max = 281.15,
        details = (
          humidity_level = 81,
          pressure_value = 1012
        )
      )
    )

    val expectedKebab = (
      `repo-info` = (
        `full-name` = "octocat/hello-world",
        `created-at` = "2011-01-26T19:01:12Z",
        owner = (
          `profile-image-url` = "http://a0.twimg.com/profile_images/1135218950/twitterapi_normal.png",
          `user-name` = "octocat"
        )
      ),
      weather = (
        `feels-like` = 278.4,
        `temp-min` = 279.15,
        `temp-max` = 281.15,
        details = (
          `humidity-level` = 81,
          `pressure-value` = 1012
        )
      )
    )

    val snake = camel.transform(_.rename(FieldName.camelCase.toSnakeCase))
    val snakeRoundrip = snake.transform(_.rename(FieldName.snakeCase.toCamelCase))
    assertEquals(snake, expectedSnake)
    assertEquals(snakeRoundrip, camel)

    val kebab = camel.transform(_.rename(FieldName.camelCase.toKebabCase))
    val kebabRoundtrip = kebab.transform(_.rename(FieldName.kebabCase.toCamelCase))
    assertEquals(kebab, expectedKebab)
    assertEquals(kebabRoundtrip, camel)

    val camelToSnakeToKebabToCamel =
      camel.transform(
        _.rename(
          FieldName.camelCase.toSnakeCase
            .andThen(FieldName.snakeCase.toCamelCase)
            .andThen(FieldName.camelCase.toKebabCase)
            .andThen(FieldName.kebabCase.toCamelCase)
        )
      )
    assertEquals(camelToSnakeToKebabToCamel, camel)
  }

  test("toplevel .merge works") {
    val tup = (
      top1 = 1,
      top2 = 2,
      top3 = (
        level1 = 1,
        level2 = 2,
        level3 = 3,
        level4 = (
          low1 = 1,
          low2 = 2,
          low3 = 3
        )
      )
    )

    val mergee = (
      top1 = "1",
      top3 = (
        level1 = "1",
        level4 = (low4 = 4),
        level5 = 123
      ),
      additional = (asd = 1)
    )

    val mergee2 = (
      top3 = (
        level4 = (oneMore = 3)
      )
    )

    val expected =
      (
        top1 = mergee.top1,
        top2 = tup.top2,
        top3 = (
          level1 = mergee.top3.level1,
          level2 = tup.top3.level2,
          level3 = tup.top3.level3,
          level4 = (
            low1 = tup.top3.level4.low1,
            low2 = tup.top3.level4.low2,
            low3 = tup.top3.level4.low3,
            low4 = mergee.top3.level4.low4,
            oneMore = mergee2.top3.level4.oneMore
          ),
          level5 = mergee.top3.level5
        ),
        additional = mergee.additional
      )

    val actual =
      tup.transform(
        _.merge(mergee),
        _.merge(mergee2)
      )

    assertEquals(expected, actual)
  }

  test("secondary merges can merge on top of each other") {
    val empty = NamedTuple.Empty

    val mergee = (
      top1 = "1",
      top3 = (
        level1 = "1",
        level4 = (low4 = 4),
        level5 = 123
      ),
      additional = (asd = 1, dsa = 2)
    )

    val mergee2 = (
      top3 = (
        level4 = (awoo = 3)
      )
    )

    val mergee3 = (additional = (asd = "OVERWRITTEN", moreFields = 3))

    val actual =
      empty.transform(_.merge(mergee), _.merge(mergee2), _.merge(mergee3))

    val expected =
      (
        top1 = mergee.top1,
        top3 = (
          level1 = mergee.top3.level1,
          level4 = (
            low4 = mergee.top3.level4.low4,
            awoo = mergee2.top3.level4.awoo
          ),
          level5 = mergee.top3.level5
        ),
        additional = (
          asd = mergee3.additional.asd,
          dsa = mergee.additional.dsa,
          moreFields = mergee3.additional.moreFields
        )
      )

    assertEquals(actual, expected)
  }

  test("merging an empty tuple -> identity") {
    val tup = (field1 = 1, field2 = 2, nested = (one = 1, two = 2))
    val actual = tup.transform(_.merge(NamedTuple.Empty))
    assertEquals(actual, tup)
  }

  test("merging on top of an empty tuple -> mergee") {
    val tup = NamedTuple.Empty
    val mergee = (field1 = 1, field2 = 2, nested = (one = 1, two = 2))
    val actual = tup.transform(_.merge(mergee))
    assertEquals(actual, mergee)
  }

  test("merging on top of a removed field should not take it into consideration") {
    val tup = (one = 1, two = 2, nested = (field1 = 1, field2 = 2))
    val mergee = (nested = (mergedField1 = 3, mergedField2 = 4))
    val actual = tup.transform(_.remove(_.nested), _.merge(mergee))
    val expected = (
      one = tup.one,
      two = tup.two,
      nested = (mergedField1 = mergee.nested.mergedField1, mergedField2 = mergee.nested.mergedField2)
    )
    assertEquals(actual, expected)
  }

  test("modifying fields that originate from a primary source is possible even when they are buried deep down in a Merged node") {
    val tup = (
      top1 = 1,
      top2 = 2,
      top3 = (
        level1 = 1,
        level2 = 2,
        level3 = 3,
        level4 = (
          low1 = 1,
          low2 = 2,
          low3 = 3,
          toBeModified = (field1 = 1)
        )
      )
    )

    val mergee = (
      top1 = "1",
      top3 = (
        level1 = "1",
        level4 = (low4 = 4),
        level5 = 123
      ),
      additional = (asd = 1)
    )

    val mergee2 = (
      top3 = (
        level4 = (oneMore = 3)
      )
    )

    val expected =
      (
        top1 = mergee.top1,
        top2 = tup.top2,
        top3 = (
          level1 = mergee.top3.level1,
          level2 = tup.top3.level2,
          level3 = tup.top3.level3,
          level4 = (
            low1 = tup.top3.level4.low1,
            low2 = tup.top3.level4.low2,
            low3 = tup.top3.level4.low3,
            toBeModified = (field1 = 101, newName = 20),
            low4 = mergee.top3.level4.low4,
            oneMore = mergee2.top3.level4.oneMore
          ),
          level5 = mergee.top3.level5
        ),
        additional = mergee.additional
      )

    val actual =
      tup.transform(
        _.merge(mergee),
        _.merge(mergee2),
        _.put(_.top3.level4.toBeModified)((newName = 20)),
        _.update(_.top3.level4.toBeModified.field1)(_ + 100)
      )

    assertEquals(actual, expected)
  }

  test(".merge.regional works") {
    val tup = (
      top1 = 1,
      top2 = 2,
      top3 = (
        level1 = 1,
        level2 = 2,
        level3 = 3,
        level4 = (
          low1 = 1,
          low2 = 2,
          low3 = 3
        )
      )
    )

    val mergee = (
        level1 = "1",
        level4 = (low4 = 4),
        level5 = 123
    )

    val expected =
      (
        top1 = tup.top1,
        top2 = tup.top2,
        top3 = (
          level1 = mergee.level1,
          level2 = tup.top3.level2,
          level3 = tup.top3.level3,
          level4 = (
            low1 = tup.top3.level4.low1,
            low2 = tup.top3.level4.low2,
            low3 = tup.top3.level4.low3,
            low4 = mergee.level4.low4,
          ),
          level5 = mergee.level5
        )
      )

    val actual =
      tup.transform(_.merge(mergee).regional(_.top3))

    assertEquals(expected, actual)
  }

}
