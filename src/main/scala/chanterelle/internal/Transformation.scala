package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap

sealed trait Transformation derives Debug {

  def calculateTpe(using Quotes): Type[?]

  final def applyModifier(modifier: Modifier)(using Quotes): Transformation = {
    def recurse(segments: List[Path.Segment], curr: Transformation)(using Quotes): Transformation = {
      import quotes.reflect.*
      (segments, curr) match {
        case (Path.Segment.Field(name = name) :: next, t: Transformation.Named) =>
          val fieldTransformation @ Transformation.OfField.FromSource(idx, transformation) =
            t.fields.getOrElse(name, report.errorAndAbort(s"No field ${name}")): @unchecked // TODO: temporary
          t.copy(fields = t.fields.updated(name, fieldTransformation.copy(transformation = recurse(next, transformation))))
        case (Path.Segment.TupleElement(index = index) :: next, t: Transformation.Tuple) =>
          t.copy(fields = t.fields.updated(index, t.fields(index)))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Optional) =>
          t.copy(paramTransformation = recurse(next, t.paramTransformation))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Collection) =>
          t.copy(paramTransformation = recurse(next, t.paramTransformation))
        case (Nil, t) => apply(modifier, t)
        case (_, t)   => report.errorAndAbort("Illegal path segment and transformation combo")
      }
    }

    def apply(modifier: Modifier, transformation: Transformation)(using Quotes): Transformation = {
      (modifier, transformation) match {
        case (m: Modifier.Add, t: Transformation.Named) =>
          val modifiedFields = m.outputStructure.fields.map((name, _) => name -> Transformation.OfField.FromModifier(m))
          val res = t.copy(
            // output = t.output.copy(fields = t.output.fields ++ m.outputStructure.fields),
            fields = t.fields ++ modifiedFields
          )
          res
        // case (m: Modifier.Compute, t: Transformation.Named) => ???
        // case (m: Modifier.Remove, t)                   => ???
        // case (m: Modifier.Update, t)                   => ???
      }
    }
    recurse(modifier.path.segments.toList, this)
  }
}

object Transformation {

  def fromStructure(structure: Structure): Transformation = {
    structure match {
      case named: Structure.Named =>
        Named(named, named.fields.map { (name, field) => name -> Transformation.OfField.FromSource(name, fromStructure(field)) })

      case tuple: Structure.Tuple =>
        Tuple(
          tuple,
          tuple.elements.zipWithIndex.map { (field, idx) => Transformation.OfField.FromSource(idx, fromStructure(field)) }
        )

      case optional: Structure.Optional =>
        Optional(optional, fromStructure(optional.paramStruct))

      case coll: Structure.Collection =>
        Collection(coll, fromStructure(coll.paramStruct))

      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    source: Structure.Named,
    fields: VectorMap[String, Transformation.OfField[String]]
  ) extends Transformation {

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case _ -> OfField.FromSource(idx, transformation)                      => transformation.calculateTpe.repr
          case name -> OfField.FromModifier(Modifier.Add(outputStructure = struct)) => struct.fields(name).tpe.repr
        }.toVector
      )

    def calculateTpe(using Quotes): Type[? <: NamedTuple.AnyNamedTuple] = {
      val values = calculateValuesTpe
      val names = calculateNamesTpe
      ((names, values): @unchecked) match {
        case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
          Type.of[NamedTuple.NamedTuple[names, values]]
      }
    }
  }

  case class Tuple(
    source: Structure.Tuple,
    fields: Vector[Transformation.OfField[Int]]
  ) extends Transformation {

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case OfField.FromSource(idx, transformation)                      => transformation.calculateTpe.repr
          case OfField.FromModifier(Modifier.Add(outputStructure = struct)) => struct.tpe.repr
        }
      )
  }

  case class Optional(
    source: Structure.Optional,
    paramTransformation: Transformation
  ) extends Transformation {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }
  }

  case class Collection(
    source: Structure.Collection,
    paramTransformation: Transformation
  ) extends Transformation {
    def calculateTpe(using Quotes): Type[? <: Iterable[?]] =
      ((source.collectionTpe, paramTransformation.calculateTpe): @unchecked) match {
        case ('[type coll[a] <: Iterable[a]; coll], '[tpe]) =>
          Type.of[coll[tpe]]
      }
  }

  case class Leaf(output: Structure.Leaf) extends Transformation {
    def calculateTpe(using Quotes): Type[? <: AnyKind] = output.tpe
  }

  enum OfField[+Idx <: Int | String] derives Debug {
    case FromSource(idx: Idx, transformation: Transformation)
    case FromModifier(modifier: Modifier) extends OfField[Nothing]
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
