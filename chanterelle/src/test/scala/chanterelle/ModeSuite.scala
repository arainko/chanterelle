package chanterelle

import munit.DisciplineSuite
import org.typelevel.discipline.*
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary
import munit.Compare
import chanterelle.interop.Collection
import scala.util.control.TailCalls.*
import scala.util.control.TailCalls

final class FailFastModeLaws[F[_]](val F: Mode.FailFast[F]) extends Laws {

  // def traverseCollectionStacksafe[Elem](using Elem: Gen[Elem], Builder: Mode[F] ?=> F.Builder[Elem, Vector[Elem]]): RuleSet = SimpleRuleSet(
  //   "mode",
  //   ""
  // )
  //

  def mapIsFlatMapPureIdentity[A, B](fa: F[A], f: A => B): Prop =
    F.map(fa, f) ?= F.flatMap(fa, f.andThen(F.pure))

  def traverseCollectionConsistency[AElem, AColl, BElem, BColl](aColl: AColl, f: AElem => F[BElem])(using
    AColl: Collection.IntoIterator[AElem, AColl],
    BColl: Mode.FailFast[F] ?=> Collection.Builder.Fallible[F, BElem, BColl]
  ): Prop = F {

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


  def apply[A, B](using A: Arbitrary[A], FA: Arbitrary[F[A]], Fn: Arbitrary[A => B], FnF: Arbitrary[A => F[B]]) = SimpleRuleSet(
    "Mode.FailFast",
    "map is consistent with flatMap and pure" -> Prop.forAll(mapIsFlatMapPureIdentity[A, B]),
    "traverseCollection is consistent with a foldLeft with a Vector accumulator" -> Prop.forAll(traverseCollectionConsistency[A, List[A], B, List[B]])
  )
}

abstract class ModeSuite[F[_]](F: Mode[F])(using Builder: Mode[F] ?=> F.Builder[Int, Vector[Int]])
    extends ChanterelleSuite,
      DisciplineSuite {

  protected given mode: Mode[F] = F

  test("big collections do not overflow the stack") {
    val vec = Vector.range(0, 1_000_000)
    val wrappedVec = vec.map(F.pure)
    val traversed = F.traverseCollection[F[Int], Int, Vector[F[Int]], Vector[Int]](wrappedVec, identity)
    val expected = F.pure(vec)
    assertEquals(traversed, expected)
  }
}

final class OptionFailFastModeSuite extends ModeSuite(Mode.FailFast.option) {
  val laws = FailFastModeLaws[Option](Mode.FailFast.option)

  checkAll("laws", laws[Int, Int])
}
final class EitherFailFastModeSuite extends ModeSuite(Mode.FailFast.either) {
  val laws = FailFastModeLaws(Mode.FailFast.either[String])

  checkAll("laws", laws[Int, Int])
}
final class EitherParallelModeSuite extends ModeSuite(Mode.Parallel.either[List[String]])
