package chanterelle.internal

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] sealed trait Structure extends scala.Product derives Debug {
  def calculateTpe(using Quotes): Type[?]

  def path: Path

  final def narrow[A <: Structure](using tt: TypeTest[Structure, A]): Option[A] = tt.unapply(this)
}

private[chanterelle] object Structure {

  def unapply(struct: Structure): Structure = struct

  case class Named(
    path: Path,
    fields: VectorMap[String, Structure]
  ) extends Structure {
    def asTuple: Structure.Tuple =
      Tuple(path, fields.values.toVector, true)

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] = 
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] = 
      rollupTuple(fields.values.map(_.calculateTpe.repr).toVector)

    def calculateTpe(using Quotes): Type[? <: NamedTuple.AnyNamedTuple] = {
      import quotes.reflect.*
      val values = calculateValuesTpe
      val names = calculateNamesTpe
      ((names, values): @unchecked) match {
        case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
          Type.of[NamedTuple.NamedTuple[names, values]]
      }
    }
  }

  case class Tuple(
    path: Path,
    elements: Vector[Structure],
    isPlain: Boolean
  ) extends Structure {
    def calculateTpe(using Quotes): Type[? <: scala.Tuple] = rollupTuple(elements.map(_.calculateTpe.repr))
  }

  case class Optional(
    path: Path,
    paramStruct: Structure
  ) extends Structure {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramStruct.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }
  }

  case class Collection(
    collectionTpe: Type[? <: Iterable],
    path: Path,
    paramStruct: Structure
  ) extends Structure {
    def calculateTpe(using Quotes): Type[? <: Iterable[?]] =
      ((collectionTpe, paramStruct.calculateTpe): @unchecked) match {
        case ('[type coll[a] <: Iterable[a]; coll], '[tpe]) =>
          Type.of[coll[tpe]]
      }
  }

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

        case '[Option[param]] =>
          Structure.Optional(
            path,
            Structure.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case '[type coll[a] <: Iterable[a]; coll[param]] =>
          Structure.Collection(
            Type.of[coll],
            path,
            Structure.of[param](
              path.appended(Path.Segment.Element(Type.of[param]))
            )
          )

        case '[type t <: NamedTuple.AnyNamedTuple; t] =>
          val valuesTpe = Type.of[NamedTuple.DropNames[t]]
          val namesTpe = Type.of[NamedTuple.Names[t]]
          val transformations =
            tupleTypeElements(valuesTpe)
              .zip(constStringTuple(namesTpe.repr))
              .map((tpe, name) =>
                name -> (tpe.asType match {
                  case '[tpe] =>
                    Structure.of[tpe](
                      path.appended(Path.Segment.Field(Type.of[tpe], name))
                    )
                })
              )
              .to(VectorMap)

          Structure.Named(path, transformations)

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
          Structure.Tuple(path, elements, isPlain = false)

        case tpe @ '[types] if tpe.repr.isTupleN =>
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

          Structure.Tuple(path, transformations, isPlain = true)

        case _ =>
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

  private def rollupTuple(using Quotes)(elements: Vector[quotes.reflect.TypeRepr]) = {
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
          .match { case '[type tpe <: scala.Tuple; tpe] => Type.of[tpe] }
      case _ =>
        val TupleCons = TypeRepr.of[*:]
        val tpe = elements.foldRight(TypeRepr.of[EmptyTuple])((curr, acc) => TupleCons.appliedTo(curr :: acc :: Nil))
        tpe.asType.match { case '[type tpe <: scala.Tuple; tpe] => Type.of[tpe] }
    }
  }
}
