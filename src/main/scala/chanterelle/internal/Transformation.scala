package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap

sealed trait Transformation derives Debug {
  val output: Structure

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
        case (m: Modifier.Add, t: Transformation.Named)     => 
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
        Named(named, named, named.fields.map { (name, field) => name -> Transformation.OfField.FromSource(name, fromStructure(field)) })

      case tuple: Structure.Tuple =>
        Tuple(tuple, tuple,  tuple.elements.zipWithIndex.map { (field, idx) => Transformation.OfField.FromSource(idx, fromStructure(field)) })

      case optional: Structure.Optional =>
        Optional(optional, optional, fromStructure(optional.paramStruct))

      case coll: Structure.Collection =>
        Collection(coll, coll, fromStructure(coll.paramStruct))

      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    source: Structure.Named,
    output: Structure.Named,
    fields: VectorMap[String, Transformation.OfField[String]]
  ) extends Transformation {
    // def output(using Quotes): Structure.Named =
    //   Structure.Named(
    //     source.path,
    //     fields.map { case (name, field) =>
    //       field match {
    //         case OfField.FromSource(_, transformation) => name -> transformation.output
    //         case OfField.FromModifier(Modifier.Add(path, str, _)) => name -> Structure.Leaf(str.fields(name).calculateTpe, path)
    //       }
    //     }
    //   )
  }

  case class Tuple(
    source: Structure.Tuple,
    output: Structure.Tuple,
    fields: Vector[Transformation.OfField[Int]]
  ) extends Transformation {
    // def output(using Quotes): Structure.Tuple =
    //   Structure.Tuple(
    //     source.path,
    //     fields.map {
    //       case OfField.FromSource(_, transformation) => transformation.output
    //       case OfField.FromModifier(Modifier.Add(path, _, _)) => ???
    //     },
    //     true
    //   )
  }

  case class Optional(
    source: Structure.Optional,
    output: Structure.Optional,
    paramTransformation: Transformation
  ) extends Transformation {
    // def output(using Quotes): Structure.Optional =
    //   Structure.Optional(
    //     source.path,
    //     paramTransformation.output
    //   )
  }

  case class Collection(
    source: Structure.Collection,
    output: Structure.Collection,
    paramTransformation: Transformation
  ) extends Transformation {
    // def output(using Quotes): Structure.Collection =
    //   Structure.Collection(
    //     source.collectionTpe,
    //     source.path,
    //     paramTransformation.output
    //   )
  }

  case class Leaf(output: Structure.Leaf) extends Transformation

  enum OfField[+Idx <: Int | String] derives Debug {
    case FromSource(idx: Idx, transformation: Transformation)
    case FromModifier(modifier: Modifier) extends OfField[Nothing]
  }
}

opaque type StructuredExpr[S <: Structure, A <: Expr[Any] & Singleton] <: S = S

object StructuredExpr {
  def create[S <: Structure, A <: Expr[Any] & Singleton](value: A, str: S): StructuredExpr[S, value.type] = str

  extension [S <: Structure, A <: Expr[Any] & Singleton](self: StructuredExpr[S, A]) {
    def accessThat(using value: ValueOf[A]) = value.value
  }
}
