package chanterelle

import chanterelle.interop.Mappable

class MappableSpec extends ChanterelleSuite {

  private case class Box[+A](value: A, tag: String)

  private given Mappable[Box] = new Mappable[Box] {
    def map[A, B](fa: Box[A], f: A => B): Box[B] = Box(f(fa.value), fa.tag)
  }

  test("update inside a custom Mappable wrapper maps over it non-fallibly (wrapper data preserved)") {
    val tup = (field = Box((x = 1), "keep me"))

    val actual = tup.transform(_.update(_.field.element.x)(_ + 1))
    val expected = (field = Box((x = 2), "keep me"))

    assertEquals(actual, expected)
  }

  test("put inside a custom Mappable wrapper") {
    val tup = (field = Box((x = 1), "t"))

    val actual = tup.transform(_.put(_.field.element)((y = 42)))
    val expected = (field = Box((x = 1, y = 42), "t"))

    assertEquals(actual, expected)
  }

  test("remove inside a custom Mappable wrapper") {
    val tup = (field = Box((x = 1, y = 2), "t"))

    val actual = tup.transform(_.remove(_.field.element.x))
    val expected = (field = Box((y = 2), "t"))

    assertEquals(actual, expected)
  }

  test("update, put and remove inside a custom Mappable wrapper in one transform") {
    val tup = (field = Box((x = 1, y = 2), "t"))

    val actual =
      tup.transform(
        _.update(_.field.element.x)(_ + 1),
        _.put(_.field.element)((z = 3)),
        _.remove(_.field.element.y)
      )

    val expected = (field = Box((x = 2, z = 3), "t"))

    assertEquals(actual, expected)
  }

  test("compute inside a custom Mappable wrapper reads the mapped source value") {
    val tup = (field = Box((x = 2), "t"))

    val actual = tup.transform(_.compute(_.field.element)(v => (doubled = v.x * 10)))
    val expected = (field = Box((x = 2, doubled = 20), "t"))

    assertEquals(actual, expected)
  }

  test("two custom Mappable wrappers are mapped independently") {
    val tup = (one = Box((x = 1), "a"), two = Box((x = 2), "b"))

    val actual =
      tup.transform(
        _.update(_.one.element.x)(_ + 10),
        _.update(_.two.element.x)(_ + 20)
      )

    val expected = (one = Box((x = 11), "a"), two = Box((x = 22), "b"))

    assertEquals(actual, expected)
  }

  test("custom Mappable wrapper nested in another custom Mappable wrapper") {
    val tup = (field = Box(Box((x = 1), "inner"), "outer"))

    val actual = tup.transform(_.update(_.field.element.element.x)(_ + 1))
    val expected = (field = Box(Box((x = 2), "inner"), "outer"))

    assertEquals(actual, expected)
  }

  test("custom Mappable wrapper inside a plain collection") {
    val tup = (list = List(Box((x = 1), "a"), Box((x = 2), "b")))

    val actual = tup.transform(_.update(_.list.each.element.x)(_ * 10))
    val expected = (list = List(Box((x = 10), "a"), Box((x = 20), "b")))

    assertEquals(actual, expected)
  }

  test("custom Mappable wrapper containing a plain collection") {
    val tup = (field = Box(List((x = 1), (x = 2)), "t"))

    val actual = tup.transform(_.update(_.field.element.each.x)(_ + 1))
    val expected = (field = Box(List((x = 2), (x = 3)), "t"))

    assertEquals(actual, expected)
  }
}
