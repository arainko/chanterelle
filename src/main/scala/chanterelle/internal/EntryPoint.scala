package chanterelle.internal

import scala.quoted.*
import chanterelle.TupleModifier
import scala.collection.BuildFrom
import scala.collection.Factory
import scala.collection.MapFactory
import scala.collection.SortedMap
import scala.collection.SortedIterableFactory
import scala.collection.IterableFactory
import scala.collection.SortedMapFactory
import scala.collection.MapOps
import scala.collection.IterableOps
import scala.collection.SortedMapOps
import scala.collection.SetOps
import scala.collection.immutable.SortedSetOps
import scala.collection.immutable.SortedSet

trait ExtractableK1[A] {
  type F[a]

  type Element

  // def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait ExtractableK2[A] {
  type F[a, b]

  type LeftElement

  type RightElement

  // def map[A, B](fa: F[A])(f: A => B): F[B]
}

object ExtractableK2 {
  given map[MapColl[k, v] <: Map[k, v], Key, Value]: ExtractableK2[Map[Key, Value]] with {
    type F[a, b] = Map[a, b]

    type LeftElement = Key

    type RightElement = Value
  }
}

object ExtractableK1 {

  
  IterableOps

  MapOps

  SortedMapOps

  given iterable[Coll[a] <: Iterable[a], Elem]: ExtractableK1[Coll[Elem]] with {
    type F[a] = Coll[a]
    type Element = Elem
  }

  // given map[MapColl[k, v] <: Map[k, v], Key, Value]: ExtractableK1[Map[Key, Value]] with {
  //   type F[a] = Iterable[a]

  //   type Element = (Key, Value)
  // }

  val dd = summon[ExtractableK2[Map[Int, Int]]]



  // summon[dd.F[Int] =:= List[Int]]

  // MapFactory

  // SortedMap()

  SortedIterableFactory

  CodePrinter.structure {
    Map(1 -> 1, 2 -> 2).map((a, b) => a -> b)
  }

  def a[K, V](using DummyImplicit)(using f: Factory[(K, V), ?]): f.type = ???

  val d = a[Int, Int]

  val a = implicitly[Factory[(Int, Int), Map[Int, Int]]]
}

object EntryPoint {
  inline def struct[A] = ${ structMacro[A] }

  def structMacro[A: Type](using Quotes) = {
    import quotes.reflect.*
    val struct = Structure.toplevel[A]
    report.info(Debug.show(struct))
    '{}
  }

  transparent inline def run[A](tuple: A, inline mods: TupleModifier.Builder[A] => TupleModifier[A]*): Any = ${ runMacro[A]('tuple, 'mods) }

  def runMacro[A: Type](tuple: Expr[A], modifications: Expr[Seq[TupleModifier.Builder[A] => TupleModifier[A]]])(using Quotes) = {
    import quotes.reflect.* 

    val structure = Structure.toplevel[A]

    val mods = Varargs.unapply(modifications).getOrElse(report.errorAndAbort("Modifications are not a simple vararg list"))

    val modifiers = Modifier.parse(mods.toList)

    val transformation = ModifiableTransformation.fromStructure(structure)

    val modifiedTransformation = modifiers.foldLeft(transformation)((acc, mod) => acc.applyModifier(mod))

    Interpreter.runTransformation(tuple, modifiedTransformation)

  }
}
