package chanterelle

import chanterelle.interop.Collection
import munit.DisciplineSuite
import org.scalacheck.Prop.*
import org.scalacheck.{ Arbitrary, Prop }
import org.typelevel.discipline.*

final class ModeLawsSpec extends DisciplineSuite {
  checkAll("Mode.FailFast for Option laws", ModeLaws.FailFast(Mode.FailFast.option)[Int, Int])
  checkAll("Mode.FailFast for Either laws", ModeLaws.FailFast(Mode.FailFast.either[String])[Int, Int])
  checkAll("Mode.Parallel for Either laws", ModeLaws.Parallel(Mode.Parallel.either[List[String]])[Int, Int, Int])
  checkAll("Mode.Accumulating for Either laws", ModeLaws.Accumulating(Mode.Accumulating.either[List[String]])[Int, Int, Int])

}

sealed abstract class ModeLaws[F[_]](val F: Mode[F]) extends Laws {
  final def traverseCollectionStackSafety(using F.Builder[Int, Vector[Int]]) = F {
    val vec = Vector.range(0, 1_000_000)
    val wrappedVec = vec.map(F.pure)
    val traversed: F[Vector[Int]] = F.traverseCollection(wrappedVec, identity[F[Int]])
    val expected = F.pure(vec)
    traversed ?= expected
  }
}

object ModeLaws {
  final class FailFast[F[_]](override val F: Mode.FailFast[F]) extends ModeLaws[F](F) {

    def apply[A, B](using Arbitrary[A], Arbitrary[F[A]], Arbitrary[A => B], Arbitrary[A => F[B]]) = SimpleRuleSet(
      "Mode.FailFast",
      "map is consistent with flatMap and pure" -> Prop.forAll(mapConsistency[A, B]),
      "traverseCollection is consistent with a foldLeft with a Vector accumulator" -> Prop.forAll(
        F { traverseCollectionConsistency[A, List[A], B, List[B]] }
      ),
      "traverseCollection is stack safe" -> F { traverseCollectionStackSafety }
    )
    private def mapConsistency[A, B](fa: F[A], f: A => B): Prop =
      F.map(fa, f) ?= F.flatMap(fa, f.andThen(F.pure))

    private def traverseCollectionConsistency[AElem, AColl, BElem, BColl](aColl: AColl, f: AElem => F[BElem])(using
      AColl: Collection.IntoIterator[AElem, AColl],
      BColl: F.Builder[BElem, BColl]
    ): Prop = {

      // this is not stack safe (:c) but .traverseCollection needs to be
      def traversedViaVector = {
        F.flatMap(
          AColl
            .iterator(aColl)
            .foldLeft(F.pure(Vector.empty[BElem])) { (acc, curr) =>
              F.flatMap(acc, acc => F.map(f(curr), curr => acc.appended(curr)))
            },
          BColl.fromSpecific
        )
      }

      F.traverseCollection(aColl, f) ?= traversedViaVector
    }

  }

  final class Parallel[F[_]](override val F: Mode.Parallel[F]) extends ModeLaws[F](F) {

    def apply[A, B, C](using
      Arbitrary[A],
      Arbitrary[F[A]],
      Arbitrary[F[B]],
      Arbitrary[F[C]],
      Arbitrary[A => B],
      Arbitrary[A => F[B]]
    ) = SimpleRuleSet(
      "Mode.Parallel",
      "map is consistent with flatMap and pure" -> Prop.forAll(mapConsistency[A, B]),
      "traverseCollection is consistent with a foldLeft with a Vector accumulator" -> Prop.forAll(
        F { traverseCollectionConsistency[A, List[A], B, List[B]] }
      ),
      "traverseCollection is stack safe" -> F { traverseCollectionStackSafety },
      "zip is associative" -> Prop.forAll(zipAssociativity[A, B, C]),
      "zip left identity" -> Prop.forAll(leftIdentity[A]),
      "zip right identity" -> Prop.forAll(rightIdentity[A])
    )

    private def zipAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]) =
      F.zip(F.zip(fa, fb), fc) ?= F.map(F.zip(fa, F.zip(fb, fc)), { case (a, (b, c)) => ((a, b), c) })

    private def leftIdentity[A](fa: F[A]) = F.map(F.zip(F.pure(()), fa), ((_, a) => a)) ?= fa

    private def rightIdentity[A](fa: F[A]) = F.map(F.zip(fa, F.pure(())), ((a, _) => a)) ?= fa

    private def mapConsistency[A, B](fa: F[A], f: A => B): Prop =
      F.map(fa, f) ?= F.flatMap(fa, f.andThen(F.pure))

    private def traverseCollectionConsistency[AElem, AColl, BElem, BColl](aColl: AColl, f: AElem => F[BElem])(using
      AColl: Collection.IntoIterator[AElem, AColl],
      BColl: F.Builder[BElem, BColl]
    ): Prop = {

      // this is not stack safe (:c) but .traverseCollection needs to be
      def traversedViaVector = {
        F.flatMap(
          AColl
            .iterator(aColl)
            .foldLeft(F.pure(Vector.empty[BElem])) { (acc, curr) =>
              F.map(F.zip(acc, f(curr)), (acc, curr) => acc.appended(curr))
            },
          BColl.fromSpecific
        )
      }

      F.traverseCollection(aColl, f) ?= traversedViaVector
    }

  }
  final class Accumulating[F[_]](override val F: Mode.Accumulating[F]) extends ModeLaws[F](F) {

    def apply[A, B, C](using
      Arbitrary[A],
      Arbitrary[F[A]],
      Arbitrary[F[B]],
      Arbitrary[F[C]],
      Arbitrary[A => F[B]]
    ) = SimpleRuleSet(
      "Mode.Accumulating",
      "traverseCollection is consistent with a foldLeft with a Vector accumulator" -> Prop.forAll(
        traverseCollectionConsistency[A, List[A], B, List[B]]
      ),
      "traverseCollection is stack safe" -> F { traverseCollectionStackSafety },
      "zip is associative" -> Prop.forAll(zipAssociativity[A, B, C]),
      "zip left identity" -> Prop.forAll(leftIdentity[A]),
      "zip right identity" -> Prop.forAll(rightIdentity[A])
    )

    private def zipAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]) =
      F.zip(F.zip(fa, fb), fc) ?= F.map(F.zip(fa, F.zip(fb, fc)), { case (a, (b, c)) => ((a, b), c) })

    private def leftIdentity[A](fa: F[A]) = F.map(F.zip(F.pure(()), fa), ((_, a) => a)) ?= fa

    private def rightIdentity[A](fa: F[A]) = F.map(F.zip(fa, F.pure(())), ((a, _) => a)) ?= fa

    private def traverseCollectionConsistency[AElem, AColl, BElem, BColl](aColl: AColl, f: AElem => F[BElem])(using
      AColl: Collection.IntoIterator[AElem, AColl],
      BColl: F.Builder[BElem, BColl]
    ): Prop = {

      // this is not stack safe (:c) but .traverseCollection needs to be
      def traversedViaVector = {
        F.map(
          AColl
            .iterator(aColl)
            .foldLeft(F.pure(Vector.empty[BElem])) { (acc, curr) =>
              F.map(F.zip(acc, f(curr)), (acc, curr) => acc.appended(curr))
            },
          BColl.fromSpecific
        )
      }

      F.traverseCollection(aColl, f) ?= traversedViaVector
    }

  }

}
