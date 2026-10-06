package chanterelle

class PutComputeHoistSpec extends ChanterelleSuite {

  private type E[A] = Either[String, A]

  private def src(inner: Int = 1, plain: Int = 99) =
    (field = Right((inner = inner)), plain = plain)

  test(".put inside a wrapper, then hoisting that wrapper") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(_.field.element)((extra = "hi")),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1, extra = "hi"), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".compute inside a wrapper, then hoisting that wrapper") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.compute(_.field.element)(v => (extra = v.inner + 1)),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1, extra = 2), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".put a wrapped value inside a wrapper, then hoisting that wrapper (nested F stays plain)") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(_.field.element)((extra = Left("nested"))),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1, extra = Left("nested")), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".compute producing a wrapped value inside a wrapper, then hoisting that wrapper") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.compute(_.field.element)(v => (extra = Right(v.inner))),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1, extra = Right(1)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapper, then .put into the now-unwrapped content") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.?(_.field),
          _.put(_.field.element)((extra = "hi"))
        )

      val expected =
        Right((field = (inner = 1, extra = "hi"), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapper, then .compute inside the now-unwrapped content") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.?(_.field),
          _.compute(_.field.element)(v => (doubled = v.inner * 2))
        )

      val expected =
        Right((field = (inner = 1, doubled = 2), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapper, then .put a wrapped value into the unwrapped content (stays plain)") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.?(_.field),
          _.put(_.field.element)((extra = Left("still here")))
        )

      val expected =
        Right((field = (inner = 1, extra = Left("still here")), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".put inside a non-hoisted wrapper while hoisting a sibling field") {
    Mode.FailFast.either[String] {
      val tup = (a = Right(1), b = Right((x = 2)), plain = 99)

      val actual =
        tup.transform(
          _.put(_.b.element)((extra = 7)),
          _.?(_.a)
        )

      val expected =
        Right((a = 1, b = Right((x = 2, extra = 7)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".compute inside a non-hoisted wrapper while hoisting a sibling field") {
    Mode.FailFast.either[String] {
      val tup = (a = Right(1), b = Right((x = 2)), plain = 99)

      val actual =
        tup.transform(
          _.compute(_.b.element)(v => (extra = v.x + 1)),
          _.?(_.a)
        )

      val expected =
        Right((a = 1, b = Right((x = 2, extra = 3)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".put at the top level (identity path), then hoist a wrapped field") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(a => a)((extra = Right(7))),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1), plain = 99, extra = Right(7)))

      assertEquals(actual, expected)
    }
  }

  test(".put, .compute and hoisting in one transform") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(_.field.element)((extra = "hi")),
          _.compute(_.field.element)(v => (derived = v.inner * 10)),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 1, extra = "hi", derived = 10), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".put alongside an earlier .update of a sibling, then hoist") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(_.field.element)((extra = "hi")),
          _.update(_.field.element.inner)(_ + 1),
          _.?(_.field)
        )

      val expected =
        Right((field = (inner = 2, extra = "hi"), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".compute reads the source value of its node, regardless of modifier order") {
    Mode.FailFast.either[String] {
      val tup = src()

      val updateThenCompute =
        tup.transform(
          _.update(_.field.element.inner)(_ + 1),
          _.compute(_.field.element)(v => (seen = v.inner))
        )

      val computeThenUpdate =
        tup.transform(
          _.compute(_.field.element)(v => (seen = v.inner)),
          _.update(_.field.element.inner)(_ + 1)
        )

      val expected =
        (field = Right((inner = 2, seen = 1)), plain = 99)

      assertEquals(updateThenCompute, expected)
      assertEquals(computeThenUpdate, expected)
    }
  }

  test(".put then .remove the containing wrapper") {
    Mode.FailFast.either[String] {
      val actual =
        src().transform(
          _.put(_.field.element)((extra = "hi")),
          _.remove(_.field)
        )

      val expected = (plain = 99)

      assertEquals(actual, expected)
    }
  }

  test("two level passthrough: .put at the deepest level, then hoist the deepest field") {
    Mode.FailFast.either[String] {
      val tup = (nest = Right(Right((keep = Right(1)))), plain = 99)

      val actual =
        tup.transform(
          _.put(_.nest.element.element)((extra = Right(9))),
          _.?(_.nest.element.element.keep)
        )

      val expected =
        Right((nest = (keep = 1, extra = Right(9)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test("hoisting a wrapped collection, then .put into its elements") {
    Mode.FailFast.either[String] {
      val tup = (list = Right(List((a = 1), (a = 2))), plain = 99)

      val actual =
        tup.transform(
          _.?(_.list),
          _.put(_.list.element.element)((b = 0))
        )

      val expected =
        Right((list = List((a = 1, b = 0), (a = 2, b = 0)), plain = 99))

      assertEquals(actual, expected)
    }
  }

  test(".put inside a wrapper that fails to unwrap - the failure propagates") {
    Mode.FailFast.either[String] {
      val tup = (field = (Left("boom"): E[(inner: Int)]), plain = 99)

      val actual =
        tup.transform(
          _.put(_.field.element)((extra = "hi")),
          _.?(_.field)
        )

      val expected = Left("boom")

      assertEquals(actual, expected)
    }
  }

  test("fields created by .put can't be referenced by later modifiers") {
    assertFailsToCompileContains {
      """
      Mode.FailFast.either[String] {
        val tup = (field = Right((inner = 1)), plain = 99)
        tup.transform(
          _.put(_.field.element)((extra = Right(5))),
          _.?(_.field.element.extra)
        )
      }
      """
    }("value extra is not a member of (inner : Int)")
  }

}
