package chanterelle.internal

import chanterelle.internal.Structure.Leaf

import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.reflect.TypeTest
import scala.collection.MapOps
import scala.collection.IterableOps

private[chanterelle] sealed trait Structure extends scala.Product derives Debug {
  def tpe: Type[?]

  def path: Path

  final def narrow[A <: Structure](using tt: TypeTest[Structure, A]): Option[A] = tt.unapply(this)

  final def asLeaf: Leaf = Structure.Leaf(tpe, path)
}

private[chanterelle] object Structure {

  def unapply(struct: Structure): Structure = struct

  sealed trait Named extends Structure derives Debug {
    def tpe: Type[? <: NamedTuple.AnyNamedTuple]
    def namesTpe: Type[? <: scala.Tuple]
    def valuesTpe: Type[? <: scala.Tuple]
    def path: Path
    def fields: VectorMap[String, Structure]
    final def asTuple: Structure.Tuple = Tuple(valuesTpe, path, fields.values.toVector)
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
    elements: Vector[Structure]
  ) extends Structure

  case class Optional(
    tpe: Type[? <: Option[?]],
    path: Path,
    paramStruct: Structure
  ) extends Structure

  case class Either(
    tpe: Type[? <: scala.Either[?, ?]],
    path: Path,
    left: Structure,
    right: Structure
  ) extends Structure

  case class Collection(
    tpe: Type[?],
    path: Path,
    repr: Collection.Repr
  ) extends Structure

  object Collection {
    enum Repr derives Debug {
      case MapLike[F[k, v]](tycon: Type[F], key: Structure, value: Structure)
      case IterLike[F[elem]](tycon: Type[F], element: Structure)
    }
  }

  case class Leaf(tpe: Type[?], path: Path) extends Structure {
    def calculateTpe(using Quotes): Type[?] = tpe
  }

  def toplevel[A: Type](using Quotes): Structure =
    Structure.of[A](Path.empty(Type.of[A]))

  def of[A: Type](path: Path)(using Quotes): Structure = {
    given Path = path // just for SupportedCollection, maybe come up with something nicer?
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

        case tpe @ '[scala.Either[e, a]] =>
          Structure.Either(
            tpe,
            path,
            Structure.of[e](
              path.appended(Path.Segment.LeftElement(Type.of[e]))
            ),
            Structure.of[a](
              path.appended(Path.Segment.RightElement(Type.of[a]))
            )
          )

        case SupportedCollection(structure) => structure

        // TODO: report to dotty: it's not possible to match on a NamedTuple type like this: 'case '[NamedTuple[names, values]] => ...', this match always fails, you need to decompose stuff like the below
        case tpe @ '[type t <: NamedTuple.AnyNamedTuple; t] =>
          val valuesTpe = Type.normalized[NamedTuple.DropNames[t]].assertBoundedBy[scala.Tuple]
          val namesTpe = Type.normalized[NamedTuple.Names[t]].assertBoundedBy[scala.Tuple]
          val transformations =
            TupleTypes
              .unroll(valuesTpe)
              .lazyZip(TupleTypes.unrollStrings(namesTpe.repr))
              .map((tpe, name) =>
                name -> (tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(Path.Segment.Field(Type.of[tpe], name))
                    )
                })
              )(using VectorMap)

          if transformations.size == 1 then
            val (fieldName, valueStructure) = transformations.head
            Structure.Named.Singular(tpe, namesTpe, valuesTpe, fieldName, valueStructure, path)
          else Structure.Named.Freeform(tpe, namesTpe, valuesTpe, path, transformations)

        case tpe @ '[Any *: scala.Tuple] if !tpe.repr.isTupleN => // let plain tuples be caught later on
          val elements =
            TupleTypes
              .unrollIndexed(tpe) { (tpe, idx) =>
                tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(Path.Segment.TupleElement(Type.of[tpe], idx))
                    )
                }
              }
          Structure.Tuple(tpe, path, elements)

        case tpe @ '[types & scala.Tuple] if tpe.repr.isTupleN =>
          val transformations =
            TupleTypes.unrollIndexed(Type.of[types])((tpe, idx) =>
              tpe.asType match {
                case '[tpe] =>
                  Structure.of[tpe](
                    path.appended(
                      Path.Segment.TupleElement(Type.of[tpe], idx)
                    )
                  )
              }
            )

          Structure.Tuple(tpe, path, transformations)

        case '[tpe] =>
          Structure.Leaf(Type.of[A], path)
      }
  }

  private object SupportedCollection {
    def unapply(tpe: Type[?])(using q: Quotes, path: Path): Option[Structure.Collection] = {
      import quotes.reflect.*
      tpe match {
        case tpe @ '[type map[k, v] <: MapOps[k, v, `map`, ?]; MapOps[key, value, `map`, ?]] =>
          Some(
            Structure.Collection(
              tpe,
              path,
              Structure.Collection.Repr.MapLike(
                Type.of[map],
                Structure.of[key](path.appended(Path.Segment.TupleElement(Type.of[key], 0))),
                Structure.of[value](path.appended(Path.Segment.TupleElement(Type.of[value], 1)))
              )
            )
          )
        case tpe @ '[type coll[a] <: IterableOps[a, `coll`, ?]; IterableOps[elem, `coll`, ?]] =>
          Some(
            Structure.Collection(
              tpe,
              path,
              Structure.Collection.Repr.IterLike(
                Type.of[coll],
                Structure.of[elem](path.appended(Path.Segment.Element(Type.of[elem])))
              )
            )
          )
        case _ => None
      }
    }
  }
}
