package chanterelle.internal

object TestParsing {
  case class Test(int: Int, str: String)


  FieldDeriver.testParsing[Test](a => a.int)

  FieldDeriver.testParsing[List[Int]](a => a)

  
}
