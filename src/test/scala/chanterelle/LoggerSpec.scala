package chanterelle.internal

import munit.FunSuite

class LoggerSpec extends FunSuite {
  test("Logger is off") {
    assertEquals(Logger.level, Logger.Level.Off)
  }
}
