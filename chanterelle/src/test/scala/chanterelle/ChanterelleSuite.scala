package chanterelle

import munit.{ FunSuite, Location }

trait ChanterelleSuite extends FunSuite {

  transparent inline def assertFailsToCompileContains(inline code: String)(head: String, tail: String*)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    (head :: tail.toList).foreach(expected => assert(errors.exists(_.contains(expected))))
  }

  transparent inline def assertFailsToCompileWith(inline code: String)(expected: String*)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    assertEquals(errors, expected.toSet, "Error did not contain expected value")
  }
}
