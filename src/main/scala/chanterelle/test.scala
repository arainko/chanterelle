package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import chanterelle.internal.CodePrinter
import scala.runtime.Tuples
import scala.reflect.TypeTest

object test extends App {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) =
    (1, 2, Some((something = (1, 2), tup = ("3", (1, 2)))))

  val easy: (name : Int, nested : (wow : Int, wow2 : Int)) = (name = 1, nested = (wow = 1, wow2 = 2))

  // val b: (String, (name: Int)) = ???

  val b =
    // EntryPoint.run(easy)
    // CodePrinter.code(
      EntryPoint.run(easy, _.add(_.nested)((newField123 = 123)))
    // )

  println(b.nested.newField123)

}
