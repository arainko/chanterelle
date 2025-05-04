package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint

extension [Labels <: Tuple, Values <: Tuple](self: NamedTuple[Labels, Values]) {
  def modify = {
    ???
  }

  inline def showStruct: Unit = EntryPoint.struct[NamedTuple[Labels, Values]]
}

object test {
  val a: (name: Int, age: Int, other: (something: (name: Int, age: Int))) =
    (name = 1, age = 2, other = (something = (name = 1, age = 2)))

  // NamedTu

  internal.Logger.locally {
    a.showStruct
  }

  val repr: Field[
    (name: Int, age: Int, other: (something: (name: Int, age: Int)))
  ] {
    val name: Field[Int]
    val age: Field[Int]
    val optional: Field[Option[String]] {
      val element: Field[String]
    }
    val other: Field[(something: (name: Int, age: Int))] {
      val something: Field[((name: Int, age: Int))] {
        val name: Field[Int]
        val age: Field[Int]
      }
    }
  } = ???

  val asd = repr.age

  def costam(a: Int ?=> String => String*) = ???

  costam(a => { summon[Int]; a.toString() }, _.strip())

  // a.modify(
  //   _.rename(_.toUpperCase.stripPrefix("asd")).local(_.a.name.element),
  //   _.rename(_.toUpperCase.stripPrefix("asd")).local(_.a.name.element),
  // )

  // a.costam
}

sealed trait Field[A] extends Selectable { self =>
  type Reprs

  inline def selectDynamic(name: String): Nothing = ???

  def update(f: A => A): Field[A] { type Repr = self.Reprs }
}
