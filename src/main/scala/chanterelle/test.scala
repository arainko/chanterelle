package chanterelle

import chanterelle.internal.EntryPoint

// import scala.NamedTuple.
import scala.collection.immutable.TreeMap
import scala.quoted.Type

//TODO: report to metals: presentation compiler crash when referring to named tuple fields inside .update and .compute
object test extends App {
  val a: (name: Int, age: Int, other: List[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) =
    (1, 2, List((something = (1, 2), tup = ("3", (1, 2)))))

  val easy: (name: Int, nested: (wow: Int, nah: Int), map: List[Int]) = (1, (2, 3), List(1))


  // val asd: TupleModifier.Builder[(name: Int, nested: (wow: Int, nah: Int))] = ???

  // EntryPoint.struct[TreeMap[Int, Int]]


  class Sumn

  val renamedB = 
    internal.CodePrinter.code {
    a.transform(
      _.remove(_.other.element.tup._2),
      // _.compute(_.other.element.tup._2) { a => 
      //   //TODO: if I uncomment 'val hmm' semantic highlighting dies and I get no autocomplete etc
      //   // val hmm = a.name123
      //   (newField123 = 231)
      // },
      // _.update(_.other.element)(a => (iHaveANameNow = a))
    )
    }
  println(renamedB)
}
