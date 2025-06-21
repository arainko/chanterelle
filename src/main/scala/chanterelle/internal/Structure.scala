package chanterelle.internal

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] sealed trait Structure extends scala.Product derives Debug {
  def tpe: Type[?]

  def path: Path

  final def narrow[A <: Structure](using tt: TypeTest[Structure, A]): Option[A] = tt.unapply(this)
}

private[chanterelle] object Structure {

  def unapply(struct: Structure): Structure = struct

  sealed trait Named extends Structure derives Debug {
    def tpe: Type[? <: NamedTuple.AnyNamedTuple]
    def namesTpe: Type[? <: scala.Tuple]
    def valuesTpe: Type[? <: scala.Tuple]
    def path: Path
    def fields: VectorMap[String, Structure]
    final def asTuple: Structure.Tuple = Tuple(valuesTpe, path, fields.values.toVector, fields.size < 23)
  }

  object Named {
    case class Freeform(
      tpe: Type[? <: NamedTuple.AnyNamedTuple],
      namesTpe: Type[? <: scala.Tuple],
      valuesTpe: Type[? <: scala.Tuple],
      path: Path,
      fields: VectorMap[String, Structure]
    ) extends Named

    case class Singular(
      tpe: Type[? <: NamedTuple.AnyNamedTuple],
      namesTpe: Type[? <: scala.Tuple],
      valuesTpe: Type[? <: scala.Tuple],
      fieldName: String,
      valueStructure: Structure,
      path: Path
    ) extends Named {
      val fields: VectorMap[String, Structure] = VectorMap(fieldName -> valueStructure)
    }

  }

  case class Tuple(
    tpe: Type[? <: scala.Tuple],
    path: Path,
    elements: Vector[Structure],
    isPlain: Boolean
  ) extends Structure {}

  case class Optional(
    tpe: Type[? <: Option[?]],
    path: Path,
    paramStruct: Structure
  ) extends Structure {}

  // TODO: This is broken, pretty please revisit it later
  case class Collection(
    tpe: Type[? <: Iterable[?]],
    collectionTpe: Type[? <: Iterable],
    path: Path,
    paramStruct: Structure
  ) extends Structure {}

  case class Leaf(tpe: Type[?], path: Path) extends Structure {
    def calculateTpe(using Quotes): Type[?] = tpe
  }

  def toplevel[A: Type](using Quotes): Structure =
    Structure.of[A](Path.empty(Type.of[A]))

  def of[A: Type](path: Path)(using Quotes): Structure = {
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

        // TODO: This is broken, pretty please revisit it later
        case tpe @ '[Iterable[param]] =>
          import quotes.reflect.*
          tpe.repr match {
            case AppliedType(tycon, _) =>
              tycon.asType match {
                case '[f] =>
                  Structure.Collection(
                    tpe,
                    Type.of[f].asInstanceOf,
                    // tpe match { case '[type coll[a]; coll[a]] => Type.of[coll] },
                    path,
                    Structure.of[param](
                      path.appended(Path.Segment.Element(Type.of[param]))
                    )
                  )
              }
          }

        // TODO: report to dotty: it's not possible to match on a NamedTuple type like this: 'case '[NamedTuple[names, values]] => ...', this match always fails, you need to decompose stuff like the below
        case tpe @ '[type t <: NamedTuple.AnyNamedTuple; t] =>
          val valuesTpe = Type.of[NamedTuple.DropNames[t]]
          val namesTpe = Type.of[NamedTuple.Names[t]]
          val transformations =
            tupleTypeElements(valuesTpe)
              .zip(constStringTuple(namesTpe.repr))
              .map((tpe, name) => //TODO: report to metals: no hover info over untupled parameter
                name -> (tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(Path.Segment.Field(Type.of[tpe], name))
                    )
                })
              )
              .to(VectorMap)

          if transformations.size == 1 then
            val (fieldName, valueStructure) = transformations.head
            Structure.Named.Singular(tpe, namesTpe, valuesTpe, fieldName, valueStructure, path)
          else Structure.Named.Freeform(tpe, namesTpe, valuesTpe, path, transformations)

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
          Structure.Tuple(tpe, path, elements, isPlain = false)

        case tpe @ '[types & scala.Tuple] if tpe.repr.isTupleN =>
          val transformations =
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

          Structure.Tuple(tpe, path, transformations, isPlain = true)

        case '[tpe] =>
          Structure.Leaf(Type.of[A], path)
      }
  }

  private def tupleTypeElements(tpe: Type[?])(using Quotes): List[quotes.reflect.TypeRepr] = {
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

  private def constStringTuple(using Quotes)(tp: quotes.reflect.TypeRepr): List[String] = {
    import quotes.reflect.*
    tupleTypeElements(tp.asType).map { case ConstantType(StringConstant(l)) => l }
  }
}
