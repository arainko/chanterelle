package chanterelle

import scala.collection.immutable.HashMap

class OpaqueCollectionSpec extends ChanterelleSuite {

  import OpaqueFixtures.*

  test("hoisting elements of an opaque custom collection works") {
    Mode.FailFast.either[String] {
      val ok = (field = OpqVec(Vector(Right(1), Right(2))))
      val failing = (field = OpqVec(Vector(Right(1), Left("boom"))))

      val actualOk = ok.transform(_.hoist(_.field.each))
      val actualFailed = failing.transform(_.hoist(_.field.each))

      val expectedOk = Right((field = OpqVec(Vector(1, 2))))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting values out of opaque map entries works") {
    Mode.FailFast.either[String] {
      val ok = (map = OpqMap(HashMap("a" -> Right(1), "b" -> Right(2))))
      val failing = (map = OpqMap(HashMap("a" -> Left("boom"), "b" -> Right(2))))

      val actualOk = ok.transform(_.hoist(_.map.each._2))
      val actualFailed = failing.transform(_.hoist(_.map.each._2))

      val expectedOk = Right((map = OpqMap(HashMap("a" -> 1, "b" -> 2))))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("update inside an opaque custom Mappable wrapper maps over it non-fallibly (wrapper data preserved)") {
    val tup = (field = OpqBox((x = 1), "keep me"))

    val actual = tup.transform(_.update(_.field.element.x)(_ + 1))
    val expected = (field = OpqBox((x = 2), "keep me"))

    assertEquals(actual, expected)
  }

  test("put inside an opaque custom Mappable wrapper") {
    val tup = (field = OpqBox((x = 1), "t"))

    val actual = tup.transform(_.put(_.field.element)((y = 42)))
    val expected = (field = OpqBox((x = 1, y = 42), "t"))

    assertEquals(actual, expected)
  }

  test("remove inside an opaque custom Mappable wrapper") {
    val tup = (field = OpqBox((x = 1, y = 2), "t"))

    val actual = tup.transform(_.remove(_.field.element.x))
    val expected = (field = OpqBox((y = 2), "t"))

    assertEquals(actual, expected)
  }

  test("update, put and remove inside an opaque custom Mappable wrapper in one transform") {
    val tup = (field = OpqBox((x = 1, y = 2), "t"))

    val actual =
      tup.transform(
        _.update(_.field.element.x)(_ + 1),
        _.put(_.field.element)((z = 3)),
        _.remove(_.field.element.y)
      )

    val expected = (field = OpqBox((x = 2, z = 3), "t"))

    assertEquals(actual, expected)
  }

  test("two opaque custom Mappable wrappers are mapped independently") {
    val tup = (one = OpqBox((x = 1), "a"), two = OpqBox((x = 2), "b"))

    val actual =
      tup.transform(
        _.update(_.one.element.x)(_ + 10),
        _.update(_.two.element.x)(_ + 20)
      )

    val expected = (one = OpqBox((x = 11), "a"), two = OpqBox((x = 22), "b"))

    assertEquals(actual, expected)
  }

  test("opaque custom Mappable wrapper nested in another opaque custom Mappable wrapper") {
    val tup = (field = OpqBox(OpqBox((x = 1), "inner"), "outer"))

    val actual = tup.transform(_.update(_.field.element.element.x)(_ + 1))
    val expected = (field = OpqBox(OpqBox((x = 2), "inner"), "outer"))

    assertEquals(actual, expected)
  }

  test("opaque custom Mappable wrapper inside a plain collection") {
    val tup = (list = List(OpqBox((x = 1), "a"), OpqBox((x = 2), "b")))

    val actual = tup.transform(_.update(_.list.each.element.x)(_ * 10))
    val expected = (list = List(OpqBox((x = 10), "a"), OpqBox((x = 20), "b")))

    assertEquals(actual, expected)
  }

  test("opaque custom Mappable wrapper containing a plain collection") {
    val tup = (field = OpqBox(List((x = 1), (x = 2)), "t"))

    val actual = tup.transform(_.update(_.field.element.each.x)(_ + 1))
    val expected = (field = OpqBox(List((x = 2), (x = 3)), "t"))

    assertEquals(actual, expected)
  }
}
