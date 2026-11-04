package chanterelle

import munit.DisciplineSuite

abstract class ModeSuite[F[_]](F: Mode[F])(using Builder: Mode[F] ?=> F.Builder[Int, Vector[Int]]) extends ChanterelleSuite, DisciplineSuite {

  private given mode: Mode[F] = F

  test("big collections do not overflow the stack") {
    val vec = Vector.range(0, 1_000_000)
    val wrappedVec = vec.map(F.pure)
    val traversed = F.traverseCollection[F[Int], Int, Vector[F[Int]], Vector[Int]](wrappedVec, identity) 
    val expected = F.pure(vec)
    assertEquals(traversed, expected)
  }
}

abstract class FailFastModeSuite[F[_]](F: Mode.FailFast[F]) extends ModeSuite(F) {
  test(".traverseCollection is equivalent to ???") {
    val vec = Vector.range(0, 100_000)


  }
  test(".map is equivalent to F.flatMap(F.pure(...), identity)") {}

}

final class OptionFailFastModeSuite extends ModeSuite(Mode.FailFast.option)
final class EitherFailFastModeSuite extends ModeSuite(Mode.FailFast.either)
final class EitherParallelModeSuite extends ModeSuite(Mode.Parallel.either[List[String]])
