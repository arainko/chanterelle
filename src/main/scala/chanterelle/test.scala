package chanterelle

import scala.NamedTuple.*
import scala.util.chaining.*
import chanterelle.internal.EntryPoint
import scala.collection.Factory
import scala.collection.SortedSet
import scala.collection.SortedMap
import scala.collection.generic.IsSeq
import scala.collection.generic.IsMap
import scala.collection.generic.IsIterable
import scala.collection.BuildFrom
import scala.collection.mutable.TreeMap
import scala.collection.immutable.IntMap
import scala.collection.immutable.HashMap
import scala.collection.immutable.BitSet

//TODO: report to metals: presentation compiler crash when referring to named tuple fields inside .update and .compute
object test extends App {
  val a: (name: Int, age: Int, other: Option[(something: (name: Int, age: Int), tup: (String, (name123: Int, name1: Int)))]) =
    (1, 2, Some((something = (1, 2), tup = ("3", (1, 2)))))

  val easy: (name: Int, nested: (wow: Int, nah: Int)) = (1, (2, 3))


  val asd: TupleModifier.Builder[(name: Int, nested: (wow: Int, nah: Int))] = ???

  // EntryPoint.struct[scala.collection.immutable.TreeMap[Int, Int]]

  val asdasdas = BitSet()

  // asdasdas.map()

  // EntryPoint.struct[BitSet]



  asd.compute(_.nested)(a => (newField123 = a.nah + 123))

  // extension [Repr](coll: Repr)(using seq: IsIterable[Repr])
  //   def intersperse[B >: seq.A, That](sep: B)(using bf: BuildFrom[Repr, B, That]): That =
  //     val seqOps = seq(coll)
  //     bf.fromSpecific(coll)(new AbstractView[B]:
  //       def iterator = new AbstractIterator[B]:
  //         val it = seqOps.iterator
  //         var intersperseNext = false
  //         def hasNext = intersperseNext || it.hasNext
  //         def next() =
  //           val elem = if intersperseNext then sep else it.next()
  //           intersperseNext = !intersperseNext && it.hasNext
  //           elem
  //     )

  val renamedB = ???
    // a.transform(
    //   _.compute(_.other.element.tup._2) { a => 
    //     //TODO: if I uncomment 'val hmm' semantic highlighting dies and I get no autocomplete etc
    //     // val hmm = a.name123
    //     (newField123 = 231)
    //   },
    //   // _.update(_.other.element)(a => (iHaveANameNow = a))
    // )
  println(renamedB)
}
