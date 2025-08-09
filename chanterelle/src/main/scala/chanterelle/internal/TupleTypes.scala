package chanterelle.internal

import scala.annotation.tailrec
import scala.collection.IterableFactory
import scala.collection.mutable.Builder
import scala.quoted.*

private[chanterelle] object TupleTypes {

  def unroll[Coll[a] <: Seq[a]](tpe: Type[?], factory: IterableFactory[Coll] = List)(using
    Quotes
  ): Coll[quotes.reflect.TypeRepr] = {
    @tailrec def loop(using
      Quotes
    )(curr: Type[?], builder: Builder[quotes.reflect.TypeRepr, Coll[quotes.reflect.TypeRepr]]): Coll[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*

      curr match {
        case '[head *: tail] =>
          builder.addOne(TypeRepr.of[head])
          loop(Type.of[tail], builder)
        case '[EmptyTuple] =>
          builder.result()
        case other =>
          report.errorAndAbort(
            s"Unexpected type (${other.repr.show}) encountered when extracting tuple type elems. This is a bug in ducktape."
          )
      }
    }

    loop(tpe, factory.newBuilder[quotes.reflect.TypeRepr])
  }

  def unrollIndexed[A](tpe: Type[?])(f: (q: Quotes) ?=> (q.reflect.TypeRepr, Int) => A)(using Quotes): Vector[A] = {
    val builder = Vector.newBuilder[A]
    @tailrec def loop(using Quotes)(curr: Type[?], idx: Int): Vector[A] = {
      import quotes.reflect.*

      curr match {
        case '[head *: tail] =>
          builder.addOne(f(TypeRepr.of[head], idx))
          loop(Type.of[tail], idx + 1)
        case '[EmptyTuple] =>
          builder.result()
        case other =>
          report.errorAndAbort(
            s"Unexpected type (${other.repr.show}) encountered when extracting tuple type elems. This is a bug in ducktape."
          )
      }
    }
    loop(tpe, 0)
  }

  def unrollStrings(using Quotes)(tp: quotes.reflect.TypeRepr): Vector[String] = {
    import quotes.reflect.*
    unroll(tp.asType, Vector).map { case ConstantType(StringConstant(l)) => l }
  }

  def rollup(using Quotes)(elements: Vector[quotes.reflect.TypeRepr]) = {
    import quotes.reflect.*

    elements.size match {
      case 0 => Type.of[EmptyTuple]
      case 1 =>
        elements.head.asType.match { case '[tpe] => Type.of[Tuple1[tpe]] }
      case size if size <= 22 =>
        defn
          .TupleClass(size)
          .typeRef
          .appliedTo(elements.toList)
          .asType
      case _ =>
        val TupleCons = TypeRepr.of[*:]
        val tpe = elements.foldRight(TypeRepr.of[EmptyTuple])((curr, acc) => TupleCons.appliedTo(curr :: acc :: Nil))
        tpe.asType
    }
  }
}
