package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import scala.language.dynamics

object test {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int))]) = ???


  a.modify(
    _.add(_.other.element.something)((newField = 1)),
  )

}
