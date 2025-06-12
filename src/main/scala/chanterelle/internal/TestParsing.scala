package chanterelle.internal

import scala.quoted.*

object TestParsing {
  case class Test(int: Int, str: String)

  def testing(using Quotes) = {
    val expr = '{}

    val b = StructuredExpr.create(expr, ??? : Structure.Named)

    b.accessThat
  }


  
}
