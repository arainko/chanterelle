package chanterelle

import munit.FunSuite
import cats.data.Chain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import scala.collection.immutable.SortedMap
import chanterelle.interop.*

class ModesSuite extends FunSuite {
  import ModesSuite.*


  given nelBuilderFailFast[A]: Collection.Builder.Fallible[FailFast, A, NonEmptyList[A]] =
    Collection.Builder.from[List[A]].transform(NonEmptyList.fromList).fallible.transformK([a] => _.toRight("empty NonEmptyList"))

  given nemBuilderFailFast[K: Ordering, V]: Collection.Builder.Fallible[FailFast, (K, V), NonEmptyMap[K, V]] =
    Collection.Builder.from[SortedMap[K, V]].transform(NonEmptyMap.fromMap).fallible.transformK([a] => _.toRight("empty NonEmptyMap"))

  given chainBuilderFailFast[A]: Collection.Builder.Fallible[FailFast, A, Chain[A]] =
    Collection.Builder
      .fromAppendable[A, Chain](Chain.empty, (chain, elem) => chain.append(elem))
      .transform(chain => if chain.isEmpty then Left("empty Chain") else Right(chain))
      .fallible


  given nelBuilderTotal[A]: Collection.Builder[A, NonEmptyList[A]] =
    Collection.Builder.from[List[A]].transform(NonEmptyList.fromListUnsafe)

  given nemBuilderTotal[K: Ordering, V]: Collection.Builder[(K, V), NonEmptyMap[K, V]] =
    Collection.Builder.from[SortedMap[K, V]].transform(NonEmptyMap.fromMapUnsafe)

  given chainBuilderTotal[A]: Collection.Builder[A, Chain[A]] =
    Collection.Builder.fromAppendable[A, Chain](Chain.empty, (chain, elem) => chain.append(elem))

  given customMapBuilderTotal[K, V]: Collection.Builder[(K, V), CustomMap[K, V]] =
    Collection.Builder.fromFactory[(K, V), Vector[(K, V)]](Vector).transform(CustomMap.apply)

  given nelBuilderParallel[A]: Collection.Builder.Fallible[Parallel, A, NonEmptyList[A]] =
    Collection.Builder
      .from[List[A]]
      .transform(NonEmptyList.fromList)
      .fallible
      .transformK([a] => _.toRight(NonEmptyList.one("empty NonEmptyList")))

  given nemBuilderParallel[K: Ordering, V]: Collection.Builder.Fallible[Parallel, (K, V), NonEmptyMap[K, V]] =
    Collection.Builder
      .from[SortedMap[K, V]]
      .transform(NonEmptyMap.fromMap)
      .fallible
      .transformK([a] => _.toRight(NonEmptyList.one("empty NonEmptyMap")))

  given customBuilderParallel[A]: Collection.Builder.Fallible[Parallel, A, Custom[A]] =
    Collection.Builder
      .fromFactory[A, Vector[A]](Vector)
      .transform(vec => if vec.sizeIs >= 2 then Right(Custom(vec)) else Left(NonEmptyList.one("Custom needs at least 2 elements")))
      .fallible

  test("FailFast: hoisting wrappers out of NonEmptyList elements uses the fallible builder") {
    given nelIntoIterator[A]: Collection.IntoIterator[A, NonEmptyList[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.monad[FailFast] {
      val ok = (nums = NonEmptyList.of((x = Right(1): FailFast[Int]), (x = Right(2): FailFast[Int])))
      val failing = (nums = NonEmptyList.of((x = Right(1): FailFast[Int]), (x = Left("boom"): FailFast[Int])))

      val actualOk = ok.transform(_.hoist(_.nums.each.x))
      val actualFailing = failing.transform(_.hoist(_.nums.each.x))

      val expectedOk: FailFast[(nums: NonEmptyList[(x: Int)])] = Right((nums = NonEmptyList.of((x = 1), (x = 2))))
      val expectedFailing: FailFast[(nums: NonEmptyList[(x: Int)])] = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("FailFast: hoisting wrappers out of NonEmptyMap entries uses the fallible builder") {
    given nemIntoIterator[K, V]: Collection.IntoIterator[(K, V), NonEmptyMap[K, V]] =
      Collection.IntoIterator.from(_.toSortedMap.iterator)

    Mode.monad[FailFast] {
      val ok = (scores = NonEmptyMap.of("a" -> (Right(1): FailFast[Int]), "b" -> (Right(2): FailFast[Int])))
      val failing = (scores = NonEmptyMap.of("a" -> (Right(1): FailFast[Int]), "b" -> (Left("boom"): FailFast[Int])))

      val actualOk = ok.transform(_.hoist(_.scores.each._2))
      val actualFailing = failing.transform(_.hoist(_.scores.each._2))

      val expectedOk: FailFast[(scores: NonEmptyMap[String, Int])] = Right((scores = NonEmptyMap.of("a" -> 1, "b" -> 2)))
      val expectedFailing: FailFast[(scores: NonEmptyMap[String, Int])] = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("FailFast: hoisting wrappers out of Chain elements surfaces the failure of the fallible builder") {
    given chainIntoIterator[A]: Collection.IntoIterator[A, Chain[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.monad[FailFast] {
      val ok = (chain = Chain[FailFast[Int]](Right(1), Right(2)))
      val empty = (chain = Chain.empty[FailFast[Int]])
      val failing = (chain = Chain[FailFast[Int]](Right(1), Left("boom")))

      val actualOk = ok.transform(_.hoist(_.chain.each))
      val actualEmpty = empty.transform(_.hoist(_.chain.each))
      val actualFailing = failing.transform(_.hoist(_.chain.each))

      val expectedOk: FailFast[(chain: Chain[Int])] = Right((chain = Chain(1, 2)))
      val expectedEmpty: FailFast[(chain: Chain[Int])] = Left("empty Chain")
      val expectedFailing: FailFast[(chain: Chain[Int])] = Left("boom")

      assertEquals(actualOk, expectedOk)
      assertEquals(actualEmpty, expectedEmpty)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Accumulating: hoisting wrapped fields out of NonEmptyList elements accumulates errors (total builder)") {
    given nelIntoIterator[A]: Collection.IntoIterator[A, NonEmptyList[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.applicative[Accumulating] {
      val ok = (nums = NonEmptyList.of((count = Valid(1): Accumulating[Int]), (count = Valid(2): Accumulating[Int])))
      val failing = (
        nums = NonEmptyList.of(
          (count = Invalid(NonEmptyList.one("one")): Accumulating[Int]),
          (count = Valid(1): Accumulating[Int]),
          (count = Invalid(NonEmptyList.one("two")): Accumulating[Int])
        )
      )

      val actualOk = ok.transform(_.hoist(_.nums.each.count))
      val actualFailing = failing.transform(_.hoist(_.nums.each.count))

      val expectedOk: Accumulating[(nums: NonEmptyList[(count: Int)])] = Valid((nums = NonEmptyList.of((count = 1), (count = 2))))
      val expectedFailing: Accumulating[(nums: NonEmptyList[(count: Int)])] = Invalid(NonEmptyList.of("one", "two"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Accumulating: hoisting wrapped values out of NonEmptyMap entries accumulates errors (total builder)") {
    given nemIntoIterator[K, V]: Collection.IntoIterator[(K, V), NonEmptyMap[K, V]] =
      Collection.IntoIterator.from(_.toSortedMap.iterator)

    Mode.applicative[Accumulating] {
      val ok = (scores = NonEmptyMap.of("a" -> (Valid(1): Accumulating[Int]), "b" -> (Valid(2): Accumulating[Int])))
      val failing = (
        scores = NonEmptyMap.of(
          "a" -> (Invalid(NonEmptyList.one("one")): Accumulating[Int]),
          "b" -> (Invalid(NonEmptyList.one("two")): Accumulating[Int])
        )
      )

      val actualOk = ok.transform(_.hoist(_.scores.each._2))
      val actualFailing = failing.transform(_.hoist(_.scores.each._2))

      val expectedOk: Accumulating[(scores: NonEmptyMap[String, Int])] = Valid((scores = NonEmptyMap.of("a" -> 1, "b" -> 2)))
      val expectedFailing: Accumulating[(scores: NonEmptyMap[String, Int])] = Invalid(NonEmptyList.of("one", "two"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Accumulating: hoisting wrapped fields out of Chain elements uses the total builder") {
    given chainIntoIterator[A]: Collection.IntoIterator[A, Chain[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.applicative[Accumulating] {
      val ok = (chain = Chain[Accumulating[Int]](Valid(1), Valid(2)))
      val failing = (chain = Chain[Accumulating[Int]](Valid(1), Invalid(NonEmptyList.one("boom"))))

      val actualOk = ok.transform(_.hoist(_.chain.each))
      val actualFailing = failing.transform(_.hoist(_.chain.each))

      val expectedOk: Accumulating[(chain: Chain[Int])] = Valid((chain = Chain(1, 2)))
      val expectedFailing: Accumulating[(chain: Chain[Int])] = Invalid(NonEmptyList.one("boom"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Accumulating: hoisting wrapped values out of a custom map-like collection accumulates errors") {
    given customMapIntoIterator[K, V]: Collection.IntoIterator[(K, V), CustomMap[K, V]] =
      Collection.IntoIterator.from(_.pairs.iterator)

    Mode.applicative[Accumulating] {
      val ok = (scores = CustomMap(Vector("a" -> (Valid(1): Accumulating[Int]), "b" -> (Valid(2): Accumulating[Int]))))
      val failing = (
        scores = CustomMap(
          Vector(
            "a" -> (Invalid(NonEmptyList.one("one")): Accumulating[Int]),
            "b" -> (Invalid(NonEmptyList.one("two")): Accumulating[Int])
          )
        )
      )

      val actualOk = ok.transform(_.hoist(_.scores.each._2))
      val actualFailing = failing.transform(_.hoist(_.scores.each._2))

      val expectedOk: Accumulating[(scores: CustomMap[String, Int])] = Valid((scores = CustomMap(Vector("a" -> 1, "b" -> 2))))
      val expectedFailing: Accumulating[(scores: CustomMap[String, Int])] = Invalid(NonEmptyList.of("one", "two"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Parallel: hoisting wrappers out of NonEmptyList elements accumulates errors (fallible builder)") {
    given nelIntoIterator[A]: Collection.IntoIterator[A, NonEmptyList[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.parallel[Parallel] {
      val ok = (nums = NonEmptyList.of((x = Right(1): Parallel[Int]), (x = Right(2): Parallel[Int])))
      val failing = (
        nums = NonEmptyList.of(
          (x = Left(NonEmptyList.one("one")): Parallel[Int]),
          (x = Right(1): Parallel[Int]),
          (x = Left(NonEmptyList.one("two")): Parallel[Int])
        )
      )

      val actualOk = ok.transform(_.hoist(_.nums.each.x))
      val actualFailing = failing.transform(_.hoist(_.nums.each.x))

      val expectedOk: Parallel[(nums: NonEmptyList[(x: Int)])] = Right((nums = NonEmptyList.of((x = 1), (x = 2))))
      val expectedFailing: Parallel[(nums: NonEmptyList[(x: Int)])] = Left(NonEmptyList.of("one", "two"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Parallel: hoisting wrappers out of NonEmptyMap entries accumulates errors (fallible builder)") {
    given nemIntoIterator[K, V]: Collection.IntoIterator[(K, V), NonEmptyMap[K, V]] =
      Collection.IntoIterator.from(_.toSortedMap.iterator)

    Mode.parallel[Parallel] {
      val ok = (scores = NonEmptyMap.of("a" -> (Right(1): Parallel[Int]), "b" -> (Right(2): Parallel[Int])))
      val failing = (
        scores = NonEmptyMap.of(
          "a" -> (Left(NonEmptyList.one("one")): Parallel[Int]),
          "b" -> (Left(NonEmptyList.one("two")): Parallel[Int])
        )
      )

      val actualOk = ok.transform(_.hoist(_.scores.each._2))
      val actualFailing = failing.transform(_.hoist(_.scores.each._2))

      val expectedOk: Parallel[(scores: NonEmptyMap[String, Int])] = Right((scores = NonEmptyMap.of("a" -> 1, "b" -> 2)))
      val expectedFailing: Parallel[(scores: NonEmptyMap[String, Int])] = Left(NonEmptyList.of("one", "two"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualFailing, expectedFailing)
    }
  }

  test("Parallel: sibling wrappers are zipped with error accumulation") {
    Mode.parallel[Parallel] {
      val ok = (one = Right(1): Parallel[Int], two = Right("two"): Parallel[String])
      val failing = (
        one = Left(NonEmptyList.one("one")): Parallel[Int],
        two = Left(NonEmptyList.one("two")): Parallel[String]
      )

      val actualOk = ok.transform(_.hoist(_.one), _.hoist(_.two))
      val actualFailing = failing.transform(_.hoist(_.one), _.hoist(_.two))

      val expectedOk: Parallel[(one: Int, two: String)] = Right((one = 1, two = "two"))

      assertEquals(actualOk, expectedOk)
      actualFailing match {
        case Left(errors) => assertEquals(errors.toList.toSet, Set("one", "two"))
        case Right(value) => fail(s"Expected the transformation to fail, but got: $value")
      }
    }
  }

  test("Parallel: total builders are lifted into the fallible mode so empty collections stay empty") {
    given chainIntoIterator[A]: Collection.IntoIterator[A, Chain[A]] =
      Collection.IntoIterator.from(_.iterator)

    Mode.parallel[Parallel] {
      val ok = (chain = Chain[Parallel[Int]](Right(1), Right(2)))
      val empty = (chain = Chain.empty[Parallel[Int]])

      val actualOk = ok.transform(_.hoist(_.chain.each))
      val actualEmpty = empty.transform(_.hoist(_.chain.each))

      val expectedOk: Parallel[(chain: Chain[Int])] = Right((chain = Chain(1, 2)))
      val expectedEmpty: Parallel[(chain: Chain[Int])] = Right((chain = Chain.empty[Int]))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualEmpty, expectedEmpty)
    }
  }

  test("Parallel: hoisting wrappers out of a custom collection uses its fallible builder and surfaces its failures") {
    given customIntoIterator[A]: Collection.IntoIterator[A, Custom[A]] =
      Collection.IntoIterator.from(_.vec.iterator)

    Mode.parallel[Parallel] {
      val ok = (custom = Custom(Vector(Right(1): Parallel[Int], Right(2): Parallel[Int])))
      val tooSmall = (custom = Custom(Vector(Right(1): Parallel[Int])))
      val failing = (custom = Custom(Vector(Right(1): Parallel[Int], Left(NonEmptyList.one("boom")): Parallel[Int])))

      val actualOk = ok.transform(_.hoist(_.custom.each))
      val actualTooSmall = tooSmall.transform(_.hoist(_.custom.each))
      val actualFailing = failing.transform(_.hoist(_.custom.each))

      val expectedOk: Parallel[(custom: Custom[Int])] = Right((custom = Custom(Vector(1, 2))))
      val expectedTooSmall: Parallel[(custom: Custom[Int])] = Left(NonEmptyList.one("Custom needs at least 2 elements"))
      val expectedFailing: Parallel[(custom: Custom[Int])] = Left(NonEmptyList.one("boom"))

      assertEquals(actualOk, expectedOk)
      assertEquals(actualTooSmall, expectedTooSmall)
      assertEquals(actualFailing, expectedFailing)
    }
  }
}

object ModesSuite {
  type FailFast[A] = Either[String, A]
  type Accumulating[A] = Validated[NonEmptyList[String], A]
  type Parallel[A] = Either[NonEmptyList[String], A]

  final case class Custom[+A](vec: Vector[A])
  final case class CustomMap[K, V](pairs: Vector[(K, V)])
}
