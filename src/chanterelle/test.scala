package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint
import scala.language.dynamics

object test {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: String *: (name123: Int) *: EmptyTuple)]) = ???


  a.modify(
    _.add(_.other.element.tup._2)(???),
  )

}
