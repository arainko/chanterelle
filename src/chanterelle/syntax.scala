package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint

sealed trait NamedTupleBuilder[Path, Tpe] {
  def add[FieldTpe <: NamedTuple.AnyNamedTuple, FieldName <: String, A](
    selector: Path => Field[FieldTpe]
  )(value: NamedTuple[FieldName *: EmptyTuple, A *: EmptyTuple]): NamedTupleBuilder[Path, Tpe] = ???
}

extension [Labels <: Tuple, Values <: Tuple](self: NamedTuple[Labels, Values]) {
  inline def modify(using path: Field.Of[NamedTuple[Labels, Values]]): NamedTupleBuilder[path.Path, NamedTuple[Labels, Values]] = {
    ???
  }

  inline def showStruct: Unit = EntryPoint.struct[NamedTuple[Labels, Values]]
}

object test {
  val a: (name: Int, age: Int, other: (something: (name: Int, age: Int))) = ???


  a.modify.add(_.other.something)((newField = 1))

  val asf = Field.Of.derived[(name: Int, age: Int, other: (something: (name: Int, age: Int)))]

  class TestClass(val int: Int, val str: String)

    // (name = 1, age = 2, other = (something = (name = 1, age = 2)))

  // NamedTu

  internal.Logger.locally {
    a.showStruct
  }

  type Sel = Field[
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
  }

  val mod: NamedTupleBuilder[Sel, (name: Int, age: Int, other: (something: (name: Int, age: Int)))] = ???


  mod
    .add(_.other)((newField = 1))
    .add(_.other.something)((newField2 = 2))

  // val asd = repr.age

  def costam(a: Int ?=> String => String*) = ???

  costam(a => { summon[Int]; a.toString() }, _.strip())
}
