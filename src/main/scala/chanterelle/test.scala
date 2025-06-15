package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import chanterelle.internal.CodePrinter
import scala.runtime.Tuples
import scala.reflect.TypeTest

object test extends App {
  val a: (name: Int, age: Int, other: List[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) =
    (1, 2, List((something = (1, 2), tup = ("3", (1, 2)))))

  val easy: (name : Int, nested : (wow: Int, nah: Int)) = (1, (2, 3))

  // val b: (String, (name: Int)) = ???

  // Map(1 -> 2, 2 -> 1).map()

  val b =
    // EntryPoint.run(easy)
    // CodePrinter.code(
      a.modify(_.add(_.other.element.tup._2)((newField123 = 123)))

    // )

  // EntryPoint.struct[scala.collection.immutable.Map[Int, Int]]

  println(b)

}
