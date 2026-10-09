package chanterelle

class FallibleModifiersSpec extends ChanterelleSuite {

  private def wrappedTup(
    keep: Either[String, Int],
    drop: Either[String, Int]
  ) = {
    val inner = (keep = keep, drop = drop)
    val wrapped = Right(inner)
    (before = 0, nest = wrapped, plain = 99)
  }

  test(".? hoisting a wrapped field moves the wrapper to the top of the result (Option mode, Some)") {
    Mode.FailFast.option {
      val tup = (field = Some((inner = 1)))

      val actual = tup.transform(_.hoist(_.field))
      val expected = Some((field = (inner = 1)))

      assertEquals(actual, expected)
    }
  }

  test(".? hoisting a wrapped field moves the wrapper to the top of the result (Option mode, None)") {
    Mode.FailFast.option {
      val tup = (field = Option.empty[(inner: Int)])

      val actual = tup.transform(_.hoist(_.field))
      val expected = None

      assertEquals(actual, expected)
    }
  }

  test("values wrapped in F are mapped over non-fallibly when they are not hoisted") {
    Mode.FailFast.option {
      val tup = (field = Some((inner = 1)))

      val actual = tup.transform(_.update(_.field.element.inner)(_ + 1))
      val expected = (field = Some((inner = 2)))

      assertEquals(actual, expected)
    }
  }

  test(".? propagates failures of hoisted fields (FailFast Either mode)") {
    Mode.FailFast.either[String] {
      val successful = (field = Right((inner = 1)))
      val failed = (field = Left("boom"))

      val actualOk = successful.transform(_.hoist(_.field))
      val actualFailed = failed.transform(_.hoist(_.field))

      val expectedOk = Right((field = (inner = 1)))
      val expectedFailed = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailed, expectedFailed)
    }
  }

  test(".? hoisting a wrapped field allows modifying what's inside of it") {
    Mode.FailFast.either[String] {
      val tup = (field = Right((inner = 1)))

      val actual =
        tup.transform(
          _.hoist(_.field),
          _.update(_.field.element.inner)(_ + 1)
        )

      val expected = Right((field = (inner = 2)))

      assertEquals(actual, expected)
    }
  }

  test("FailFast mode stops at the first failing element of a hoisted collection") {
    Mode.FailFast.either[String] {
      val tup = (list = List(Right(1), Left("boom"), Left("bang")))

      val actual = tup.transform(_.hoist(_.list.element))
      val expected = Left("boom")

      assertEquals(actual, expected)
    }
  }

  test("Accumulating mode gathers errors from all hoisted fields") {
    Mode.Accumulating.either[List, String] {

      val tup = (
        one = Left(List("one")),
        two = Left(List("two"))
      )

      val actual = tup.transform(_.hoist(_.one), _.hoist(_.two))
      val expected = Left(List("one", "two"))

      assertEquals(actual, expected)
    }
  }

  test("Accumulating mode gathers errors from all failing elements of a hoisted collection") {
    Mode.Accumulating.either[List, String] {

      val tup = (list = List(Right(1), Left(List("boom")), Right(2), Left(List("bang"))))

      val actual = tup.transform(_.hoist(_.list.element))
      val expected = Left(List("boom", "bang"))

      assertEquals(actual, expected)
    }
  }

  test(".? hoisting through nested wrappers flattens them (Accumulating Either mode)") {
    Mode.Accumulating.either[List, String] {

      val tup = (nest = Right(Right((leaf = Right(1)))))

      val actual = tup.transform(_.hoist(_.nest.element.element.leaf))
      val expected = Right((nest = (leaf = 1)))

      assertEquals(actual, expected)
    }
  }

  test(".? reports hoisting a value that isn't wrapped in F as an error") {
    assertFailsToCompileContains {
      """
      Mode.FailFast.option {
        val tup = (field = 1)
        tup.transform(_.hoist(_.field))
      }
      """
    }("Couldn't traverse transformation plan, expected wrapped value but encountered ordinary value at _.field")
  }

  test("hoisting (passthrough) then removing a sibling inside the same subtree") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.hoist(_.nest.element.keep),
          _.remove(_.nest.element.drop)
        )

      val expected =
        Right((before = 0, nest = (keep = 1), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("removing a sibling then hoisting through the same subtree") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.remove(_.nest.element.drop),
          _.hoist(_.nest.element.keep)
        )

      val expected =
        Right((before = 0, nest = (keep = 1), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("hoisting (passthrough) then removing an unrelated top-level field") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.hoist(_.nest.element.keep),
          _.remove(_.before)
        )

      val expected =
        Right((nest = (keep = 1, drop = Right(2)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("removing the wrapper field after hoisting through it") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.hoist(_.nest.element.keep),
          _.remove(_.nest)
        )

      val expected = (before = 0, plain = 99)

      assertEquals(actual, expected)
    }
  }

  test("removal inside a wrapper without any hoisting (control)") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual = tup.transform(_.remove(_.nest.element.drop))

      val expected =
        (before = 0, nest = Right((keep = Right(1))), plain = 99)

      assertEquals(actual, expected)
    }
  }

  test("two level passthrough hoist + removal in the deepest subtree") {
    Mode.FailFast.either[String] {

      val deep =
        Right(Right((keep = Right(1), drop = Right(2))))

      val tup = (nest = deep, plain = 99)

      val actual =
        tup.transform(
          _.hoist(_.nest.element.element.keep),
          _.remove(_.nest.element.element.drop)
        )

      val expected =
        Right((nest = (keep = 1), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("removing a field from inside the hoisted target's content") {
    Mode.FailFast.either[String] {

      val inner = (k1 = 1, k2 = 2)
      val keepVal = Right(inner)
      val tup = (nest = Right((keep = keepVal)), plain = 99)

      val actual =
        tup.transform(
          _.hoist(_.nest.element.keep),
          _.remove(_.nest.element.keep.element.k2)
        )

      val expected =
        Right((nest = (keep = (k1 = 1)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("removing the hoisted target itself after hoisting it") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.hoist(_.nest.element.keep),
          _.remove(_.nest.element.keep)
        )

      val expected =
        Right((before = 0, nest = (drop = Right(2)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("removing a field then hoisting its sibling through the same wrapper") {
    Mode.FailFast.either[String] {
      val tup = wrappedTup(Right(1), Right(2))

      val actual =
        tup.transform(
          _.remove(_.nest.element.keep),
          _.hoist(_.nest.element.drop)
        )

      val expected =
        Right((before = 0, nest = (drop = 2), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("option mode - hoisting (passthrough) then removing a sibling inside the subtree") {
    Mode.FailFast.option {
      val inner = (keep = Some(1), drop = Some(2))
      val tup = (nest = Some(inner), plain = 99)

      val actual =
        tup.transform(
          _.hoist(_.nest.some.keep),
          _.remove(_.nest.some.drop)
        )

      val expected =
        Some((nest = (keep = 1), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".hoist.local works") {
    Mode.FailFast.option {
      val tup = (one = Some(1), two = Some(2), three = (nested1 = Some(3), nested2 = Some(2), nested3 = Some(3)))

      val actualTargeted = tup.transform(_.hoist.local(_.three))
      val actualDefault = tup.transform(_.hoist.local)
      val actual123 = tup.transform(_.hoist.regional(a => a))

      val expectedDefault= Some((one = 1 , two = 2, three = (nested1 = Some(3), nested2 = Some(2), nested3 = Some(3))))
      val expectedTargeted = Some((one = Some(1), two = Some(2), three = (nested1 = 3, nested2 = 2, nested3 = 3)))

      assertEquals(actualTargeted, expectedTargeted)
      assertEquals(actualDefault, expectedDefault)
    }

  }

  test("BUG: traversing a field removed earlier in the same transform should be reported as an error (fallible hoist)") {
    assertFailsToCompileContains {
      """
      Mode.FailFast.either[String] {
        val wrapped =
          Right((keep = Right(1), drop = Right(2)))
        val tup = (before = 0, nest = wrapped, plain = 99)
        tup.transform(
          _.remove(_.nest),
          _.hoist(_.nest.element.keep)
        )
      }
      """
    }("No field 'nest' found")
  }

}
