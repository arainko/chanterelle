package chanterelle.internal
import chanterelle.internal.Structure.Leaf
import chanterelle.interop.Collection.IntoIterator
import chanterelle.interop.Mappable

import scala.annotation.unused
import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.reflect.TypeTest

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
      case MapLike[F[_, _]](
        tycon: Type[F],
        isColl: Erased.K2[IntoIterator],
        key: Structure,
        value: Structure
      )
      case IterLike[F[_]](tycon: Type[F], isColl: Erased.K2[IntoIterator], element: Structure)
    }
  }

  case class Wrapped[F[_]](
    tpe: Type[?], // <-- it's supposed to be F[underlying.tpe]
    wrapper: WrapperType[F],
    path: Path,
    mappable: Expr[Mappable[F]],
    wrapped: Structure
  ) extends Structure

  case class Leaf(tpe: Type[?], path: Path) extends Structure {
    def calculateTpe(using Quotes): Type[?] = tpe
  }

  def toplevel[A: Type](using Quotes, Context.Any): Structure =
    Structure.of[A](Path.empty(Type.of[A]))

  def of[A: Type](path: Path)(using Quotes, Context.Any): Structure = {
    given Path = path // just for SupportedCollection, maybe come up with something nicer?
    Logger.loggedInfo("Structure"):
      Type.of[A] match {
        case tpe @ '[Nothing] =>
          Structure.Leaf(tpe, path)

        case WrappedType(Res(wrapper = wrapper: WrapperType[f], wrapped = '[wrapped], mappable = m)) =>
          @unused given Type[f] = wrapper.wrapper
          Structure.Wrapped[f](
            Type.of[f[wrapped]],
            wrapper,
            path,
            m,
            Structure.of[wrapped](path.appended(Path.Segment.Element(Type.of[wrapped])))
          )

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
    def unapply(tpe: Type[?])(using q: Quotes, path: Path, context: Context.Any): Option[Structure.Collection] = {
      Type.unapplied(tpe).flatMap {
        case '[type map[k, v]; map] -> ('[key] :: '[value] :: Nil) =>
          Expr.summon[IntoIterator[(key, value), map[key, value]]].map { isColl =>
            Structure.Collection(
              tpe,
              path,
              Structure.Collection.Repr.MapLike(
                Type.of[map],
                Erased.K2(isColl),
                Structure.of[key](path.appended(Path.Segment.TupleElement(Type.of[key], 0))),
                Structure.of[value](path.appended(Path.Segment.TupleElement(Type.of[value], 1)))
              )
            )
          }

        case '[type coll[a]; coll] -> ('[elem] :: Nil) =>
          Expr.summon[IntoIterator[elem, coll[elem]]].map { isColl =>
            Structure.Collection(
              tpe,
              path,
              Structure.Collection.Repr.IterLike(
                Type.of[coll],
                Erased.K2(isColl),
                Structure.of[elem](path.appended(Path.Segment.Element(Type.of[elem])))
              )
            )
          }
        case _ => None
      }
    }
  }

  private case class Res[F[_]](wrapper: WrapperType[F], wrapped: Type[?], mappable: Expr[Mappable[F]])

  private object WrappedType {
    def unapply(
      tpe: Type[?]
    )(using
      q: Quotes,
      context: Context.Any
    ): Option[Res[?]] = {
      def mappableCandidate = Type
        .unapplied(tpe)
        .collect {
          case (tycon = '[type f[_]; f], args = wrapped :: Nil) =>
            Expr
              .summon[Mappable[f]]
              .map(mappable => Res(wrapper = WrapperType.create[f], wrapped = wrapped, mappable = mappable))
        }
        .flatten

      context match {
        case ctx: Context.PossibleFallible[?, ?] =>
          ctx.wrapperType
            .unapply(tpe)
            .map((wrapper, wrapped) => Res(wrapper = wrapper, wrapped = wrapped, mappable = ctx.mode.value))
            .orElse(mappableCandidate)
        case Context.Total => mappableCandidate
      }
    }

  }
}
