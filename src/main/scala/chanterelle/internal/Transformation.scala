package chanterelle.internal

import scala.annotation.{tailrec}
import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] sealed trait Transformation extends scala.Product
    derives Debug {
  def tpe: Type[?]

  def path: Path

  def _1: Type[?] = tpe

  final def narrow[A <: Transformation](using
      tt: TypeTest[Transformation, A]
  ): Option[A] = tt.unapply(this)

  final def applyModifier(modifier: Modifier)(using Quotes): Transformation = {
    def apply(modifier: Modifier, transformation: Transformation) = {
      (modifier, transformation) match {
        case (m: Modifier.Add, t: Transformation.Named) => 
        case (m: Modifier.Compute, t: Transformation.Named) => 
        case (m: Modifier.Remove, t) =>
        case (m: Modifier.Update, t) =>
      }
      
    }

    def recurse(segments: List[Path.Segment], curr: Transformation)(using Quotes): Transformation = {
      import quotes.reflect.*
      (segments, curr) match {
        case (Path.Segment.Field(name = name) :: next, t: Transformation.Named) => 
          val fieldTransformation = t.fields.getOrElse(name, report.errorAndAbort(s"No field ${name}"))
          t.copy(fields = t.fields.updated(name, recurse(next, fieldTransformation)))
        case (Path.Segment.TupleElement(index = index) :: next, t: Transformation.Tuple) => 
          t.copy(elements = t.elements.updated(index, t.elements(index)))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Optional) => 
          t.copy(paramStruct = recurse(next, t.paramStruct))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Collection) => 
          t.copy(paramStruct = recurse(next, t.paramStruct)) 
        case (Nil, t) => report.errorAndAbort(s"Arrived at: ${t.show}")
        case (_, t) => report.errorAndAbort("Illegal path segment to transformation combo")
      }
    }
    recurse(modifier.path.segments.toList, this)
    ???
  }
}

private[chanterelle] object Transformation {
  def unapply(struct: Transformation): Transformation = struct

  case class Named(
      tpe: Type[? <: NamedTuple.AnyNamedTuple],
      path: Path,
      fields: VectorMap[String, Transformation]
  ) extends Transformation

  case class Tuple(
      tpe: Type[?],
      path: Path,
      elements: Vector[Transformation],
      isPlain: Boolean
  ) extends Transformation

  case class Optional(
      tpe: Type[? <: Option[?]],
      path: Path,
      paramStruct: Transformation
  ) extends Transformation

  case class Collection(
      tpe: Type[? <: Iterable[?]],
      path: Path,
      paramStruct: Transformation
  ) extends Transformation

  // case class Modified(tpe: Type[?], path: Path, modifier: Modifier) extends Transformation

  case class Leaf(tpe: Type[?], path: Path) extends Transformation

  def toplevel[A: Type](using Quotes): Transformation =
    Transformation.of[A](Path.empty(Type.of[A]))

  def of[A: Type](path: Path)(using Quotes): Transformation = {
    import quotes.reflect.*

    Logger.loggedInfo("Structure"):
      Type.of[A] match {
        case tpe @ '[Nothing] =>
          Transformation.Leaf(tpe, path)

        case tpe @ '[Option[param]] =>
          Transformation.Optional(
            tpe,
            path,
            Transformation.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case tpe @ '[Iterable[param]] =>
          Transformation.Collection(
            tpe,
            path,
            Transformation.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case '[type t <: NamedTuple.AnyNamedTuple; t] =>
          val transformations =
            tupleTypeElements(Type.of[NamedTuple.DropNames[t]])
              .zip(constStringTuple(TypeRepr.of[NamedTuple.Names[t]]))
              .map((tpe, name) =>
                name -> (tpe.asType match {
                  case '[tpe] =>
                    Transformation.of[tpe](
                      path.appended(Path.Segment.Field(Type.of[tpe], name))
                    )
                })
              )
              .to(VectorMap)

          Transformation.Named(Type.of[t], path, transformations)

        case tpe @ '[Any *: scala.Tuple] if !tpe.repr.isTupleN => // let plain tuples be caught later on
          val elements =
            tupleTypeElements(tpe).zipWithIndex.map { (tpe, idx) =>
              tpe.asType match {
                case '[tpe] =>
                  Transformation.of[tpe](
                    path.appended(Path.Segment.TupleElement(Type.of[tpe], idx))
                  )
              }
            }.toVector
          Transformation.Tuple(Type.of[A], path, elements, isPlain = false)

        case tpe @ '[types] if tpe.repr.isTupleN =>
          val transformations =
            tupleTypeElements(Type.of[types]).zipWithIndex
              .map((tpe, idx) =>
                tpe.asType match {
                  case '[tpe] =>
                    Transformation.of[tpe](
                      path.appended(
                        Path.Segment.TupleElement(Type.of[tpe], idx)
                      )
                    )
                }
              )
              .toVector

          Transformation.Tuple(Type.of[A], path, transformations, isPlain = true)

        case _ =>
          Transformation.Leaf(Type.of[A], path)
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
