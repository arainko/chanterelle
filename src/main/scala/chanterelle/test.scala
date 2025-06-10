package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import chanterelle.internal.CodePrinter
import scala.runtime.Tuples
import scala.reflect.TypeTest

object test extends App {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) = 
    (1, 2, Some((something = (1, 2), tup = ("3", (1, 2)))))


  // val easy = (name = 1, name2 = 2, name3 = 3)

  // val b: (String, (name: Int)) = ???




  val b = 
    // EntryPoint.run(easy)
    a.modify(_.add(_.other.element.something)((newField123 = 123)))



  println(b)

}
