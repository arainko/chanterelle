package chanterelle

import cats.data.*
import cats.data.Validated.{ Invalid, Valid }
import chanterelle.interop.*
import munit.{ DisciplineSuite, FunSuite }
import org.scalacheck.{ Arbitrary, Gen }

import scala.collection.immutable.SortedMap

final class CatsModeLawsSuite extends DisciplineSuite {
  given [A: Arbitrary as A, B: Arbitrary as B]: Arbitrary[ValidatedNel[A, B]] = Arbitrary {
    for {
      list <- Gen.nonEmptyListOf(A.arbitrary).map(NonEmptyList.fromListUnsafe)
      either <- Gen.either(list, B.arbitrary)
    } yield Validated.fromEither(either)
  }

  checkAll("Mode.parallel", ModeLaws.Parallel(Mode.parallel[[a] =>> Either[List[String], a]])[Int, Int, Int])
  checkAll("Mode.monad", ModeLaws.FailFast(Mode.monad[Option])[Int, Int])
  checkAll("Mode.applicative for Either", ModeLaws.Accumulating(Mode.applicative[[a] =>> Either[List[String], a]])[Int, Int, Int])
  checkAll(
    "Mode.applicative for Validated",
    ModeLaws.Accumulating(Mode.applicative[[a] =>> ValidatedNel[String, a]])[Int, Int, Int]
  )
}

