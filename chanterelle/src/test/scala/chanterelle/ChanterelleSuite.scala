package chanterelle

import munit.{ FunSuite, Location }

trait ChanterelleSuite extends FunSuite {

  transparent inline def assertCompiles(inline code: String)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    assert(clue(errors).isEmpty, s"Expected code to compile, but got errors:\n${errors.mkString("\n")}")
  }

  transparent inline def assertFailsToCompileContains(inline code: String)(head: String, tail: String*)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    (head :: tail.toList).foreach(expected => assert(clue(errors).exists(_.contains(expected))))
  }

  transparent inline def assertFailsToCompileWith(inline code: String)(expected: String*)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    assertEquals(errors, expected.toSet, "Error did not contain expected value")
  }
}
