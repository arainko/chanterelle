package chanterelle.internal

import chanterelle.internal.Plan.IsModified

import scala.collection.Factory
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import scala.util.boundary
import scala.util.boundary.Label

private[chanterelle] enum Transformation derives Debug {
  case Named(
    source: Structure.Named,
    fields: VectorMap[String, Transformation.Field],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )

  case Tuple(
    source: Structure.Tuple,
    fields: SortedMap[Int, Transformation],
    outputTpe: Type[?]
  )

  case Optional(
    source: Structure.Optional,
    paramTransformation: Transformation,
    outputTpe: Type[? <: Option[?]]
  )

  case EitherLike(
    source: Structure.Either,
    left: Transformation,
    right: Transformation,
    outputTpe: Type[? <: scala.Either[?, ?]]
  )

  case MapLike[F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[F],
    key: Transformation,
    value: Transformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case IterLike[F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[F],
    elem: Transformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case Leaf(output: Structure.Leaf)

  case ConfedUp(config: Configured)

  case Merged(
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, Transformation.Merged.Field],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )
}

private[chanterelle] object Transformation {

  def create(transformation: Plan[Nothing])(using Quotes): scala.Either[ErrorMessage, Transformation] = {
    def transformField(field: Plan.Field[Nothing])(using Label[ErrorMessage]): Field =
      field match {
        case Plan.Field.FromSource(srcName, plan) =>
          Field.FromSource(srcName, recurse(plan))
        case Plan.Field.FromModifier(mod) =>
          Field.FromModifier(mod)
      }

    def fromMerged(plan: Plan.Merged[Nothing])(using Quotes, Label[ErrorMessage]): Transformation.Merged = {
      val fields =
        plan.fields.collect {
          case (name, Plan.Merged.Field.FromPrimary(source, field, false)) =>
            name -> Transformation.Merged.Field.FromPrimary(source, transformField(field))
          case (name, Plan.Merged.Field.FromSecondary(secName, ref, accessibleFrom, plan)) =>
            val transformation: Transformation.Leaf | Transformation.Merged = plan match {
              case leaf: Plan.Leaf              => Transformation.fromLeaf(leaf)
              case merged: Plan.Merged[Nothing] => fromMerged(merged)
            }
            name -> Transformation.Merged.Field.FromSecondary(secName, ref, accessibleFrom, transformation)
        }
      Merged(
        plan.mergees,
        fields,
        plan.calculateNamesTpe,
        plan.calculateValuesTpe
      )
    }

    def recurse(transformation: Plan[Nothing])(using Label[ErrorMessage]): Transformation =
      transformation match {
        // optimization: if a Transformation hasn't been modified it's valid to just treat it as a Leaf (i.e. rewrite the source value)
        case plan if plan.isModified == IsModified.No =>
          val tpe = plan.calculateTpe
          Leaf(Structure.Leaf(tpe, Path.empty(tpe))) // TODO: figure out what to do about the path here

        case p @ Plan.Named(source, fields, _) =>
          Named(
            source,
            fields.transform((_, field) => transformField(field)),
            p.calculateNamesTpe,
            p.calculateValuesTpe
          )

        case p @ Plan.Tuple(source, fields, _) =>
          Tuple(source, fields.map((idx, plan) => idx -> recurse(plan)), p.calculateTpe)

        case p @ Plan.Optional(source, paramTransformation, _) =>
          Optional(source, recurse(paramTransformation), p.calculateTpe)

        case p @ Plan.Either(source, left, right, _) =>
          EitherLike(source, recurse(left), recurse(right), p.calculateTpe)

        case p @ Plan.MapLike(source, key, value, _) =>
          val tpe = p.calculateTpe
          val factory = (source.tycon, tpe).runtimeChecked match {
            case ('[type map[k, v]; map], '[collection.Map[key, value]]) =>
              Expr.summon[Factory[(key, value), map[key, value]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }
          MapLike(source, recurse(key), recurse(value), factory, tpe)

        case t @ Plan.IterLike(source, elem, _) =>
          val tpe = t.calculateTpe
          val factory = (source.tycon, tpe).runtimeChecked match {
            case ('[type coll[a]; coll], '[Iterable[elem]]) =>
              Expr.summon[Factory[elem, coll[elem]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }
          IterLike(source, recurse(elem), factory, tpe)

        case p: Plan.Merged[Nothing] =>
          fromMerged(p)

        case leaf: Plan.Leaf =>
          Transformation.fromLeaf(leaf)

        case Plan.ConfedUp(config, _) =>
          ConfedUp(config)
      }

    boundary[Transformation | ErrorMessage](recurse(transformation)) match {
      case transformation: Transformation => Right(transformation)
      case error: ErrorMessage            => Left(error)
    }
  }

  def fromLeaf(leaf: Plan.Leaf): Transformation.Leaf = Transformation.Leaf(leaf.output)

  enum Field derives Debug {
    case FromSource(name: String, transformation: Transformation)
    case FromModifier(modifier: Configured.NamedSpecific)

  }

  object Merged {
    enum Field derives Debug {
      case FromPrimary(source: Structure.Named, underlying: Transformation.Field)
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        transformation: Leaf | Merged
      )
    }
  }
}
