package chanterelle

class OpacityModeSpec extends ChanterelleSuite {

  import OpaqueFixtures.*

  test("hoisting a wrapped field moves the opaque wrapper to the top of the result (Some)") {
    OpqOpt.failFast {
      val tup = (field = OpqOpt(Some((inner = 1))))

      val actual = tup.transform(_.hoist(_.field))
      val expected = OpqOpt(Some((field = (inner = 1))))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapped field moves the opaque wrapper to the top of the result (None)") {
    OpqOpt.failFast {
      val tup = (field = OpqOpt.empty[(inner: Int)])

      val actual = tup.transform(_.hoist(_.field))
      val expected = OpqOpt.empty

      assertEquals(actual, expected)
    }
  }

  test("values wrapped in the opaque mode type are mapped over non-fallibly when they are not hoisted") {
    OpqOpt.failFast {
      val tup = (field = OpqOpt(Some((inner = 1))))

      val actual = tup.transform(_.update(_.field.element.inner)(_ + 1))
      val expected = (field = OpqOpt(Some((inner = 2))))

      assertEquals(actual, expected)
    }
  }

  test("hoisting elements of a plain collection inside the opaque mode works (all Some)") {
    OpqOpt.failFast {
      val ok = (list = List(OpqOpt(Some((inner = 1))), OpqOpt(Some((inner = 2)))))
      val failing = (list = List(OpqOpt(Some((inner = 1))), OpqOpt(None)))

      val actualOk = ok.transform(_.hoist(_.list.each))
      val actualFailed = failing.transform(_.hoist(_.list.each))

      val expectedOk = OpqOpt(Some((list = List((inner = 1), (inner = 2)))))
      val expectedFailed = OpqOpt(None)

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting wrapped opaque values through the opaque parallel Either mode works when all fields succeed") {
    OpqEither.parallel {
      val tup =
        (one = OpqEither(Right(OpqBox((inner = 1), "a"))), two = OpqEither(Right(OpqBox((inner = 2), "b"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected =
        OpqEither(Right((one = OpqBox((inner = 1), "a"), two = OpqBox((inner = 2), "b"))))

      assertEquals(actual, expected)
    }
  }

  test("hoisting wrapped opaque values through the opaque parallel Either mode reports failure when a field failed") {
    OpqEither.parallel {
      val tup =
        (one = OpqEither(Left(List("one"))), two = OpqEither(Right(OpqBox((inner = 2), "b"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected = OpqEither(Left(List("one")))

      assertEquals(actual, expected)
    }
  }

  test("hoisting wrapped opaque values through the opaque parallel Either mode used as an accumulating mode gathers all errors") {
    OpqEither.parallel {
      val tup = (one = OpqEither(Left(List("one"))), two = OpqEither(Left(List("two"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected = OpqEither(Left(List("one", "two")))

      assertEquals(actual, expected)
    }
  }

  test("hoisting elements of an opaque custom collection through the opaque parallel Either mode") {
    OpqEither.parallel {
      val ok = (vec = OpqVec(Vector(OpqEither(Right(1)), OpqEither(Right(2)))))
      val failing = (vec = OpqVec(Vector(OpqEither(Right(1)), OpqEither(Left(List("boom"))))))

      val actualOk = ok.transform(_.hoist(_.vec.each))
      val actualFailed = failing.transform(_.hoist(_.vec.each))

      val expectedOk = OpqEither(Right((vec = OpqVec(Vector(1, 2)))))
      val expectedFailed = OpqEither(Left(List("boom")))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test("hoisting wrapped opaque values through the opaque accumulating mode works when all fields succeed") {
    OpqEitherAcc.accumulating {
      val tup =
        (one = OpqEitherAcc(Right(OpqBox((inner = 1), "a"))), two = OpqEitherAcc(Right(OpqBox((inner = 2), "b"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected =
        OpqEitherAcc(Right((one = OpqBox((inner = 1), "a"), two = OpqBox((inner = 2), "b"))))

      assertEquals(actual, expected)
    }
  }

  test("hoisting wrapped opaque values through the opaque accumulating mode reports failure when a field failed") {
    OpqEitherAcc.accumulating {
      val tup =
        (one = OpqEitherAcc(Left(List("one"))), two = OpqEitherAcc(Right(OpqBox((inner = 2), "b"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected = OpqEitherAcc(Left(List("one")))

      assertEquals(actual, expected)
    }
  }

  test("hoisting wrapped opaque values through the opaque accumulating mode gathers all errors") {
    OpqEitherAcc.accumulating {
      val tup = (one = OpqEitherAcc(Left(List("one"))), two = OpqEitherAcc(Left(List("two"))))

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected = OpqEitherAcc(Left(List("one", "two")))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapped field moves the opaque accumulating wrapper to the top of the result (Right)") {
    OpqEitherAcc.accumulating {
      val tup = (field = OpqEitherAcc(Right((inner = 1))))

      val actual = tup.transform(_.hoist(_.field))
      val expected = OpqEitherAcc(Right((field = (inner = 1))))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapped field moves the opaque accumulating wrapper to the top of the result (Left)") {
    OpqEitherAcc.accumulating {
      val tup = (field = OpqEitherAcc(Left(List("boom"))))

      val actual = tup.transform(_.hoist(_.field))
      val expected = OpqEitherAcc(Left(List("boom")))

      assertEquals(actual, expected)
    }
  }

  test("hoisting elements of an opaque custom collection through the opaque accumulating mode") {
    OpqEitherAcc.accumulating {
      val ok = (vec = OpqVec(Vector(OpqEitherAcc(Right(1)), OpqEitherAcc(Right(2)))))
      val failing = (vec = OpqVec(Vector(OpqEitherAcc(Right(1)), OpqEitherAcc(Left(List("boom"))))))

      val actualOk = ok.transform(_.hoist(_.vec.each))
      val actualFailed = failing.transform(_.hoist(_.vec.each))

      val expectedOk = OpqEitherAcc(Right((vec = OpqVec(Vector(1, 2)))))
      val expectedFailed = OpqEitherAcc(Left(List("boom")))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }
}
