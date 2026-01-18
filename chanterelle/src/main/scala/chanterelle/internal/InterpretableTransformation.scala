package chanterelle.internal

import chanterelle.internal.Transformation.IsModified

import scala.collection.Factory
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import scala.util.boundary
import scala.util.boundary.Label

private[chanterelle] enum InterpretableTransformation derives Debug {
  case Named(
    source: Structure.Named,
    fields: VectorMap[String, InterpretableTransformation.OfField],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )

  case Tuple(
    source: Structure.Tuple,
    fields: SortedMap[Int, InterpretableTransformation],
    outputTpe: Type[?]
  )

  case Optional(
    source: Structure.Optional,
    paramTransformation: InterpretableTransformation,
    outputTpe: Type[? <: Option[?]]
  )

  case Either(
    source: Structure.Either,
    left: InterpretableTransformation,
    right: InterpretableTransformation,
    outputTpe: Type[? <: scala.Either[?, ?]]
  )

  case MapLike[F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[F],
    key: InterpretableTransformation,
    value: InterpretableTransformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case IterLike[F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[F],
    elem: InterpretableTransformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case Leaf(output: Structure.Leaf)

  case ConfedUp(config: Configured)

  case Merged(
    source: Structure.Named,
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, InterpretableTransformation.Merged.Field],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )
}

object InterpretableTransformation {

  def create(transformation: Transformation[Nothing])(using Quotes): scala.Either[ErrorMessage, InterpretableTransformation] = {
    def transformField(ofField: Transformation.Field[Nothing])(using Label[ErrorMessage]): OfField =
      ofField match {
        case Transformation.Field.FromSource(srcName, t) =>
          OfField.FromSource(srcName, recurse(t))
        case Transformation.Field.FromModifier(mod) =>
          OfField.FromModifier(mod)
      }

    def recurse(transformation: Transformation[Nothing])(using Label[ErrorMessage]): InterpretableTransformation =
      transformation match {
        // optimization: if a Transformation hasn't been modified it's valid to just treat it as a Leaf (i.e. rewrite the source value)
        case t if t.isModified == IsModified.No =>
          val tpe = t.calculateTpe
          Leaf(Structure.Leaf(tpe, Path.empty(tpe))) // TODO: figure out what to do about the path here
        case t @ Transformation.Named(source, fields, _) =>
          Named(
            source,
            fields.transform((_, field) => transformField(field)),
            t.calculateNamesTpe,
            t.calculateValuesTpe
          )
        case t @ Transformation.Tuple(source, fields, _) =>
          Tuple(source, fields.map((idx, t) => idx -> recurse(t)), t.calculateTpe)
        case t @ Transformation.Optional(source, paramTransformation, _) =>
          Optional(source, recurse(paramTransformation), t.calculateTpe)
        case t @ Transformation.Either(source, left, right, _) =>
          Either(source, recurse(left), recurse(right), t.calculateTpe)

        case t @ Transformation.MapLike(source, key, value, _) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type map[k, v]; map], '[collection.Map[key, value]]) =>
              Expr.summon[Factory[(key, value), map[key, value]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }

          MapLike(source, recurse(key), recurse(value), factory, tpe)
        case t @ Transformation.IterLike(source, elem, _) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type coll[a]; coll], '[Iterable[elem]]) =>
              Expr.summon[Factory[elem, coll[elem]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }
          IterLike(source, recurse(elem), factory, tpe)

        case t @ Transformation.Merged(source, mergees, fields) =>
          val tFields =
            fields.collect {
              case (name, Transformation.Merged.Field.FromPrimary(ofField, false)) =>
                name -> InterpretableTransformation.Merged.Field.FromPrimary(transformField(ofField))
              case (name, Transformation.Merged.Field.FromSecondary(secName, ref, accessibleFrom, transformation)) =>
                name -> InterpretableTransformation.Merged.Field.FromSecondary(secName, ref, accessibleFrom, ???)
            }
          Merged(
            source,
            mergees,
            tFields,
            t.calculateNamesTpe,
            t.calculateValuesTpe
          )
        case Transformation.Leaf(output) =>
          Leaf(output)
        case Transformation.ConfedUp(config, _) =>
          ConfedUp(config)
      }

    boundary[InterpretableTransformation | ErrorMessage](recurse(transformation)) match {
      case transformation: InterpretableTransformation => Right(transformation)
      case error: ErrorMessage                         => Left(error)
    }
  }

  enum OfField derives Debug {
    case FromSource(name: String, transformation: InterpretableTransformation)
    case FromModifier(modifier: Configured.NamedSpecific)

  }

  object Merged {
    enum Field {
      case FromPrimary(underlying: InterpretableTransformation.OfField)
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        transformation: Leaf | Merged
      )
    }
  }
}
