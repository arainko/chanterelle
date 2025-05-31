package chanterelle

import scala.NamedTuple.*
import chanterelle.internal.EntryPoint

sealed trait Modifier[Path <: Selector[Tpe], Tpe] {
  def add[FieldTpe <: NamedTuple.AnyNamedTuple, FieldName <: String, A](
    selector: Path => Selector[FieldTpe]
  )(value: NamedTuple[FieldName *: EmptyTuple, A *: EmptyTuple]): Modifier[Path, Tpe] = ???

}

extension [Labels <: Tuple, Values <: Tuple](self: NamedTuple[Labels, Values]) {
  def modify = {
    ???
  }

  inline def showStruct: Unit = EntryPoint.struct[NamedTuple[Labels, Values]]
}

object test {
  val a: (name: Int, age: Int, other: (something: (name: Int, age: Int))) = ???
    // (name = 1, age = 2, other = (something = (name = 1, age = 2)))

  // NamedTu

  internal.Logger.locally {
    a.showStruct
  }

  type Sel = Selector[
    (name: Int, age: Int, other: (something: (name: Int, age: Int)))
  ] {
    val name: Selector[Int]
    val age: Selector[Int]
    val optional: Selector[Option[String]] {
      val element: Selector[String]
    }
    val other: Selector[(something: (name: Int, age: Int))] {
      val something: Selector[((name: Int, age: Int))] {
        val name: Selector[Int]
        val age: Selector[Int]
      }
    }
  }

  val mod: Modifier[Sel, (name: Int, age: Int, other: (something: (name: Int, age: Int)))] = ???


  mod
    .add(_.other)((newField = 1))
    .add(_.other.something)((newField2 = 2))

  // val asd = repr.age

  def costam(a: Int ?=> String => String*) = ???

  costam(a => { summon[Int]; a.toString() }, _.strip())

  // a.modify(
  //   _.rename(_.toUpperCase.stripPrefix("asd")).local(_.a.name.element),
  //   _.rename(_.toUpperCase.stripPrefix("asd")).local(_.a.name.element),
  // )

  // a.costam


  type Unbuild[A] =
    A match {
      case NamedTuple[labels, types] => Tuple.Zip[labels, Tuple.Map[types, Unbuild]]
      case _ => A
    }

  type Build[LabelsAndTypes <: Tuple, OutLabels <: Tuple, OutTypes <: Tuple] =
    LabelsAndTypes match {
      case (label, Option[tpe]) *: tail => 
        Build[tail, label *: OutLabels, Build[("element", tpe) *: EmptyTuple, EmptyTuple, EmptyTuple] *: OutTypes]
      case (label, Iterable[tpe]) *: tail => Nothing
      case (label, tpe) *: tail => 
        Build[tail, label *: OutLabels, tpe *: OutTypes]
      case EmptyTuple => NamedTuple[OutLabels, OutTypes]
    }



  type Test = Build[("int", Int) *: ("opt", Option[Int]) *: EmptyTuple, EmptyTuple, EmptyTuple]

  type Unbuilt = Unbuild[(name: Int, str: String, other: (someOtherField: Int, field23: Int))]
}

sealed trait Selector[A] extends Selectable { self =>
  
  def selectDynamic(name: String): Nothing = ???
}
