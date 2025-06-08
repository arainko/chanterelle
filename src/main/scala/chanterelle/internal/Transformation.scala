package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap

sealed trait Transformation derives Debug {
  final def applyModifier(modifier: Modifier)(using Quotes): Transformation = {
    def recurse(segments: List[Path.Segment], curr: Transformation)(using Quotes): Transformation = {
      import quotes.reflect.*
      (segments, curr) match {
        case (Path.Segment.Field(name = name) :: next, t: Transformation.Named) =>
          val fieldTransformation  @ Transformation.OfField.FromSource(idx, transformation) =
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

    def apply(modifier: Modifier, transformation: Transformation): Transformation = {
      (modifier, transformation) match {
        case (m: Modifier.Add, t: Transformation.Named)     => 
          val modifiedFielfs = m.outputStructure.fields.map( )
        case (m: Modifier.Compute, t: Transformation.Named) => ???
        case (m: Modifier.Remove, t)                   => ???
        case (m: Modifier.Update, t)                   => ???
      }

    }
    recurse(modifier.path.segments.toList, this)
    ???
  }
}

object Transformation {

  def fromStructure(structure: Structure): Transformation = {
    structure match {
      case named: Structure.Named =>
        Named(named, named.fields.map { (name, field) => name -> Transformation.OfField.FromSource(name, fromStructure(field)) })

      case tuple: Structure.Tuple =>
        Tuple(tuple, tuple.elements.zipWithIndex.map { (field, idx) => Transformation.OfField.FromSource(idx, fromStructure(field)) })

      case optional: Structure.Optional =>
        Optional(optional, fromStructure(optional.paramStruct))

      case coll: Structure.Collection =>
        Collection(coll, fromStructure(coll.paramStruct))

      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    structure: Structure.Named,
    fields: VectorMap[String, Transformation.OfField[String]]
  ) extends Transformation

  case class Tuple(
    structure: Structure.Tuple,
    fields: Vector[Transformation.OfField[Int]]
  ) extends Transformation

  case class Optional(
    structure: Structure.Optional,
    paramTransformation: Transformation
  ) extends Transformation

  case class Collection(
    structure: Structure.Collection,
    paramTransformation: Transformation
  ) extends Transformation

  case class Leaf(structure: Structure.Leaf) extends Transformation

  enum OfField[+Idx <: Int | String] derives Debug {
    case FromSource(idx: Idx, transformation: Transformation)
    case FromModifier(modifier: Modifier)
  }
}
