package chanterelle

import chanterelle.hidden.Selector

class CollectionSpec extends ChanterelleSuite {

  test("hoisting a wrapped field of every element escapes through the collection (FailFast)") {
    Mode.FailFast.either[String] {
      val ok = (list = List((name = "a", count = Right(1)), (name = "b", count = Right(2))))
      val failing = (list = List((name = "a", count = Left("boom")), (name = "b", count = Left("bang"))))

      val actualOk = ok.transform(_.hoist(_.list.element.count))
      val actualFailed = failing.transform(_.hoist(_.list.element.count))

      val expectedOk = Right((list = List((name = "a", count = 1), (name = "b", count = 2))))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting wrapped fields out of elements accumulates errors (Accumulating)") {
    Mode.Accumulating.either[List, String] {
      val tup = (list =
        List[(count: Either[List[String], Int])]((count = Left(List("one"))), (count = Right(1)), (count = Left(List("two"))))
      )

      val actual = tup.transform(_.hoist(_.list.each.count))
      val expected = Left(List("one", "two"))

      assertEquals(actual, expected)
    }
  }

  test("hoisting wrappers out of nested collections") {
    Mode.FailFast.either[String] {
      val ok = (list = List(List(Right(1), Right(2))))
      val failing = (list = List(List(Left("boom"), Right(2))))

      val actualOk = ok.transform(_.hoist(_.list.each.each))
      val actualFailed = failing.transform(_.hoist(_.list.each.each))

      val expectedOk = Right((list = List(List(1, 2))))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting values out of map entries") {
    Mode.FailFast.either[String] {
      val ok = (map = Map("a" -> Right(1), "b" -> Right(2)))
      val failing = (map = Map("a" -> Left("boom"), "b" -> Right(2)))

      val actualOk = ok.transform(_.hoist(_.map.each._2))
      val actualFailed = failing.transform(_.hoist(_.map.each._2))

      val expectedOk = Right((map = Map("a" -> 1, "b" -> 2)))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting a wrapper that contains a collection, then modifying its elements") {
    Mode.FailFast.either[String] {
      val tup = (list = Right(List((x = 1), (x = 2))))

      val actual =
        tup.transform(
          _.hoist(_.list),
          _.update(_.list.element.each.x)(_ * 10)

        )

      val expected = Right((list = List((x = 10), (x = 20))))

      assertEquals(actual, expected)
    }
  }

  test("modifying elements of a plain collection while hoisting a sibling wrapper") {
    Mode.FailFast.either[String] {
      val tup = (list = List((x = 1), (x = 2)), flag = Right(9))

      val actual =
        tup.transform(
          _.hoist(_.flag),
          _.update(_.list.each.x)(_ + 1)
        )

      val expected = Right((list = List((x = 2), (x = 3)), flag = 9))

      assertEquals(actual, expected)
    }
  }

  test("removing a field from every element while hoisting a sibling wrapper") {
    Mode.FailFast.either[String] {
      val tup = (list = List((x = 1, y = 2)), flag = Right(9))

      val actual =
        tup.transform(
          _.hoist(_.flag),
          _.remove(_.list.each.y)
        )

      val expected = Right((list = List((x = 1)), flag = 9))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapper around a collection AND wrapped fields inside its elements (inside + outside)") {
    Mode.FailFast.either[String] {
      val ok = (list = Right(List((a = 1, b = Right(2)), (a = 2, b = Right(3)))))
      val failingInner = (list = Right(List[(a: Int, b: Either[String, Int])]((a = 1, b = Left("boom")), (a = 2, b = Right(3)))))
      val failingOuter = (list = (Left("outer"): Either[String, List[(a: Int, b: Either[String, Int])]]))

      val actualOk = ok.transform(_.hoist(_.list), _.hoist(_.list.element.each.b))
      val actualInner = failingInner.transform(_.hoist(_.list), _.hoist(_.list.element.each.b))
      val actualOuter = failingOuter.transform(_.hoist(_.list), _.hoist(_.list.element.each.b))

      val expectedOk = Right((list = List((a = 1, b = 2), (a = 2, b = 3))))
      val expectedInner = Left("boom")
      val expectedOuter = Left("outer")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualInner, expectedInner)
      assertEquals(actualOuter, expectedOuter)
    }
  }

  test("non-fallible values inside elements are mapped while an outside wrapper is hoisted") {
    Mode.FailFast.either[String] {
      val tup = (list = List(Some(1), None), flag = Right(9))

      val actual =
        tup.transform(
          _.hoist(_.flag),
          _.update(_.list.each.some)(_ + 1)
        )

      val expected = Right((list = List(Some(2), None), flag = 9))

      assertEquals(actual, expected)
    }
  }

  test("hoisting elements of custom collections works") {
    case class CusVector[+A](vec: Vector[A])

    given [A]: chanterelle.interop.Collection.IntoIterator[A, CusVector[A]] =
      chanterelle.interop.Collection.IntoIterator.from(_.vec.iterator)

    given [A]: chanterelle.interop.Collection.Builder[A, CusVector[A]] =
      chanterelle.interop.Collection.Builder.from(Vector).transform(CusVector.apply)

    Mode.FailFast.either[String] {
      val ok = (field = CusVector(Vector(Right(1), Right(2))))
      val failing = (field = CusVector(Vector(Right(1), Left("boom"))))

      val actualOk = ok.transform(_.hoist(_.field.each))
      val actualFailed = failing.transform(_.hoist(_.field.each))

      val expectedOk = Right((field = CusVector(Vector(1, 2))))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting elements in Option mode (Some / None)") {
    Mode.FailFast.option {
      val ok = (list = List(Some((inner = 1))))
      val failing = (list = List(Some((inner = 1)), None))

      val actualOk = ok.transform(_.hoist(_.list.each))
      val actualFailed = failing.transform(_.hoist(_.list.each))

      val expectedOk = Some((list = List((inner = 1))))
      val expectedFailed = None

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }
}
