package chanterelle.internal


import munit.FunSuite
import scala.quoted.*
import scala.deriving.Mirror
class LoggerSpec extends chanterelle.ChanterelleSuite {
  test("Logger is off") {

    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    val a = staging.withQuotes {
      Debug.show((field1 = Some((field1 = 2, field3 = 3)), field2 = 2, field3 = 3, field4 = 4))
    }

    val b = summon[Mirror.ProductOf[(field1: Int, field2: Int)]]

    println(a)

    // assertEquals(Logger.level, Logger.Level.Off)
  }
}
