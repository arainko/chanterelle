package chanterelle

import scala.NamedTuple.*

object test extends App {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) =
    (1, 2, Some((something = (1, 2), tup = ("3", (1, 2)))))

  val easy: (name: Int, nested: (wow: Int, nah: Int)) = (1, (2, 3))

  val renamedB =
    a.transform(
      _.add(_.other.element.tup._2)((newField123 = 123)),
      _.update(_.other.element)(a => (iHaveANameNow = a))
    )

  println(renamedB)
}
