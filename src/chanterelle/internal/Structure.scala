package chanterelle.internal

import scala.annotation.{tailrec, unused}
import scala.collection.immutable.VectorMap
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] sealed trait Structure extends scala.Product
    derives Debug {
  def tpe: Type[?]

  def path: Path

  def _1: Type[?] = tpe

  final def narrow[A <: Structure](using
      tt: TypeTest[Structure, A]
  ): Option[A] = tt.unapply(this)
}

private[chanterelle] object Structure {
  def unapply(struct: Structure): Structure = struct

  def toplevelAny(using Quotes) =
    Structure.Leaf(Type.of[Any], Path.empty(Type.of[Any]))

  def toplevelNothing(using Quotes) =
    Structure.Leaf(Type.of[Nothing], Path.empty(Type.of[Nothing]))

  case class Named(
      tpe: Type[?],
      path: Path,
      fields: VectorMap[String, Structure]
  ) extends Structure

  case class Tuple(
      tpe: Type[?],
      path: Path,
      elements: Vector[Structure],
      isPlain: Boolean
  ) extends Structure

  case class Optional(
      tpe: Type[? <: Option[?]],
      path: Path,
      paramStruct: Structure
  ) extends Structure

  case class Collection(
      tpe: Type[? <: Iterable[?]],
      path: Path,
      paramStruct: Structure
  ) extends Structure

  case class Leaf(tpe: Type[?], path: Path) extends Structure

  def fromTypeRepr(using
      Quotes
  )(repr: quotes.reflect.TypeRepr, path: Path): Structure =
    repr.widen.asType match {
      case '[tpe] => Structure.of[tpe](path)
    }

  def toplevel[A: Type](using Quotes): Structure =
    Structure.of[A](Path.empty(Type.of[A]))

  def of[A: Type](path: Path)(using Quotes): Structure = {
    import quotes.reflect.*

    Logger.info(
      (TypeRepr.of[Int] <:< TypeRepr.of[scala.NamedTuple.AnyNamedTuple])
        .toString()
    )

    Logger.loggedInfo("Structure"):
      Type.of[A] match {
        case tpe @ '[Nothing] =>
          Structure.Leaf(tpe, path)

        case tpe @ '[Option[param]] =>
          Structure.Optional(
            tpe,
            path,
            Structure.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case tpe @ '[Iterable[param]] =>
          Structure.Collection(
            tpe,
            path,
            Structure.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case tpe @ '[type t <: NamedTuple.AnyNamedTuple; t] =>
          val structures =
            tupleTypeElements(Type.of[NamedTuple.DropNames[t]])
              .zip(constStringTuple(TypeRepr.of[NamedTuple.Names[t]]))
              .map((tpe, name) =>
                name -> (tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(Path.Segment.Field(Type.of[tpe], name))
                    )
                })
              )
              .to(VectorMap)

          Structure.Named(Type.of[A], path, structures)

        case tpe @ '[Any *: scala.Tuple] if !tpe.repr.isTupleN => // let plain tuples be caught later on
          val elements =
            tupleTypeElements(tpe).zipWithIndex.map { (tpe, idx) =>
              tpe.asType match {
                case '[tpe] =>
                  Structure.of[tpe](
                    path.appended(Path.Segment.TupleElement(Type.of[tpe], idx))
                  )
              }
            }.toVector
          Structure.Tuple(Type.of[A], path, elements, isPlain = false)

        case tpe @ '[types] if tpe.repr.isTupleN =>
          val structures =
            tupleTypeElements(Type.of[types]).zipWithIndex
              .map((tpe, idx) =>
                tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(
                        Path.Segment.TupleElement(Type.of[tpe], idx)
                      )
                    )
                }
              )
              .toVector

          Structure.Tuple(Type.of[A], path, structures, isPlain = true)

        case tpe =>
          Structure.Leaf(Type.of[A], path)
      }
  }

  private def tupleTypeElements(
      tpe: Type[?]
  )(using Quotes): List[quotes.reflect.TypeRepr] = {
    @tailrec def loop(using
        Quotes
    )(
        curr: Type[?],
        acc: List[quotes.reflect.TypeRepr]
    ): List[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*

      curr match {
        case '[head *: tail] =>
          loop(Type.of[tail], TypeRepr.of[head] :: acc)
        case '[EmptyTuple] =>
          acc
        case other =>
          report.errorAndAbort(
            s"Unexpected type (${other.repr.show}) encountered when extracting tuple type elems. This is a bug in chanterelle."
          )
      }
    }

    loop(tpe, Nil).reverse
  }

  private def constStringTuple(using
      Quotes
  )(tp: quotes.reflect.TypeRepr): List[String] = {
    import quotes.reflect.*
    tupleTypeElements(tp.asType).map { case ConstantType(StringConstant(l)) =>
      l
    }
  }
}
