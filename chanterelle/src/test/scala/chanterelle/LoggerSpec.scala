package chanterelle.internal


import munit.FunSuite
import scala.quoted.*
import scala.deriving.Mirror
class LoggerSpec extends chanterelle.ChanterelleSuite {
  test("Logger is off") {
    assertEquals(Logger.level, Logger.Level.Off)
  }
}
