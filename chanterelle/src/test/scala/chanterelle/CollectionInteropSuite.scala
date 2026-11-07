package chanterelle

import chanterelle.interop.Collection.{ Builder, IntoIterator }
import munit.Location

import scala.collection.immutable.HashMap

class CollectionInteropSuite extends ChanterelleSuite {

  test("IntoIterator.iterableOnce works") {
    val list = IntoIterator.iterableOnce[Int, List]
    assertEquals(list.iterator(List(1, 2, 3)).toList, List(1, 2, 3))
    assertEquals(list.iterator(List.empty[Int]).toList, Nil)

    val vector = IntoIterator.iterableOnce[Int, Vector]
    assertEquals(vector.iterator(Vector(1, 2)).toVector, Vector(1, 2))
    assertEquals(vector.iterator(Vector.empty).toVector, Vector.empty)

    val set = IntoIterator.iterableOnce[Int, Set]
    assertEquals(set.iterator(Set(1)).toSet, Set(1))
    assertEquals(set.iterator(Set.empty).toSet, Set.empty)

    val iter = IntoIterator.iterableOnce[Int, Iterator]
    assertEquals(iter.iterator(List(1, 2).iterator).toList, List(1, 2))
    assertEquals(iter.iterator(Iterator.empty).toList, Nil)
  }

  test("IntoIterator for Map works") {
    val map = summon[IntoIterator[(String, Int), Map[String, Int]]]
    assertEquals(map.apply(Map("a" -> 1, "b" -> 2)).iterator.toMap, Map("a" -> 1, "b" -> 2))
  }

  test("IntoIterator.from works") {
    val str = IntoIterator.from[Char, String](_.iterator)
    assertEquals(str.iterator("abc").toList, List('a', 'b', 'c'))
  }

  test("default implicit Builder instances for Vector, Map, Set work") {
    assertEquals(summon[Builder[Int, Vector[Int]]].fromSpecific(Iterator(1)), Vector(1))
    assertEquals(summon[Builder[(String, Int), Map[String, Int]]].fromSpecific(Iterator("a" -> 1)), Map("a" -> 1))
    assertEquals(summon[Builder[String, Set[String]]].fromSpecific(Iterator("a", "b")), Set("a", "b"))
  }

  test("Builder.from can be used with only the first type arg (and infers the second one)") {
    val b = Builder.from[List[Int]]
    assertEquals(b.newBuilder.addOne(1).result(), List(1))
  }

  test("Builder.transform works") {
    val vec = summon[Builder[Int, List[Int]]].transform(_.toVector)
    assertEquals(vec.fromSpecific(Iterator(1)), Vector(1))
    assertEquals(vec.newBuilder.addOne(2).result(), Vector(2))
  }

  test("Builder.fromAppendable works") {
    val b = Builder.fromAppendable[Int, Vector](Vector.empty, _ :+ _)
    assertEquals(b.fromSpecific(Iterator(1, 2)), Vector(1, 2))
    assertEquals(b.newBuilder.addAll(List(3)).result(), Vector(3))
  }

  test("Builder.Fallible.fromTotal works (given a Mode in scope)") {
    Mode.FailFast.option {
      val b = Builder.Fallible.fromTotal[Option, Int, List[Int]]
      assertEquals(b.fromSpecific(Iterator(1, 2)), Some(List(1, 2)))
      assertEquals(b.newBuilder.addAll(List(3)).result(), Some(List(3)))
    }
  }

  test("Builder.Fallible.transformK works") {
    Mode.FailFast.option {
      val b = Builder.Fallible
        .fromTotal[Option, Int, List[Int]]
        .transformK[[a] =>> Either[String, a]]([a] => (o: Option[a]) => o.map(Right(_)).getOrElse(Left("none")))
      assertEquals(b.fromSpecific(Iterator(1)), Right(List(1)))
    }
  }

  test("IntoIterator -> Builder roundtrips") {
    def roundTrip[Elem, Coll](coll: Coll)(using it: IntoIterator[Elem, Coll], builder: Builder[Elem, Coll], loc: Location) = {
      val nb = builder.newBuilder
      it.iterator(coll).foreach(nb += _)
      val roundtripped = nb.result()
      assertEquals(roundtripped, coll)
    }

    roundTrip(List(1, 2, 3))
    roundTrip(Vector(1, 2))
    roundTrip(Set('a', 'b'))
    roundTrip(Map("a" -> 1, "b" -> 2))
    roundTrip(HashMap("a" -> 1, "b" -> 2))
  }
}
