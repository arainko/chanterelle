package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import chanterelle.internal.CodePrinter

object test extends App {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) = 
    (1, 2, Some((something = (1, 2), tup = ("3", (1, 2)))))

  // val b: (String, (name: Int)) = ???


  val b = 
    CodePrinter.code(a.modify())

    


  println(b)


}
