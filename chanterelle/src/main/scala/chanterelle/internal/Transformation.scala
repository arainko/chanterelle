package chanterelle.internal

import chanterelle.internal.Plan.IsModified

import scala.collection.Factory
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import scala.util.boundary
import scala.util.boundary.Label
import chanterelle.internal.Plan.IsHoisted
import chanterelle.internal.Context.Total
import chanterelle.internal.Context.PossiblyFallible

private[chanterelle] enum Transformation[+F <: Fallible] derives Debug {
  case Named(
    source: Structure.Named,
    fields: VectorMap[String, Transformation.Field[F]],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )

  case Tuple(
    source: Structure.Tuple,
    fields: SortedMap[Int, Transformation[F]],
    outputTpe: Type[?]
  )

  case Optional(
    source: Structure.Optional,
    paramTransformation: Transformation[F],
    outputTpe: Type[? <: Option[?]]
  )

  case EitherLike(
    source: Structure.Either,
    left: Transformation[F],
    right: Transformation[F],
    outputTpe: Type[? <: scala.Either[?, ?]]
  )

  case MapLike[+F <: Fallible, map[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[map],
    key: Transformation[F],
    value: Transformation[F],
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  ) extends Transformation[F]

  case IterLike[+F <: Fallible, iter[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[iter],
    elem: Transformation[F],
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  ) extends Transformation[F]

  case Leaf(output: Structure.Leaf) extends Transformation[Nothing]

  case ConfedUp(config: Configured[F])

  case Merged(
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, Transformation.Merged.Field[F]],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple]
  )

  case Wrapped[+F <: Fallible, G[_]](
    source: Structure.Wrapped[G],
    wrapped: Transformation[F],
    outputTpe: Type[?],
    isHoisted: Transformation.IsHoisted
  ) extends Transformation[Fallible]

}

private[chanterelle] object Transformation {
  enum IsHoisted derives Debug {
    case Yes, No
  }

  def create[F <: Fallible](
    transformation: Plan[Nothing]
  )(using Quotes, Context.Of[F]): scala.Either[ErrorMessage, Transformation[F]] = {
    def transformField[F <: Fallible](field: Plan.Field[Nothing])(using Label[ErrorMessage], Context.Of[F]): Field[F] =
      field match {
        case Plan.Field.FromSource(srcName, plan) =>
          Field.FromSource(srcName, recurse(plan))
        case Plan.Field.FromModifier(mod) =>
          Field.FromModifier(mod)
      }

    def fromMerged[F <: Fallible](
      plan: Plan.Merged[Nothing]
    )(using Quotes, Label[ErrorMessage], Context.Of[F]): Transformation.Merged[F] = {
      val fields =
        plan.fields.collect {
          case (name, Plan.Merged.Field.FromPrimary(source, field, false)) =>
            name -> Transformation.Merged.Field.FromPrimary(source, transformField(field))
          case (name, Plan.Merged.Field.FromSecondary(secName, ref, accessibleFrom, plan)) =>
            val transformation: Transformation.Leaf | Transformation.Merged[F] = plan match {
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

    def recurse[F <: Fallible](transformation: Plan[Nothing])(using Label[ErrorMessage], Context.Of[F]): Transformation[F] =
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

        case p: Plan.Wrapped[Nothing, f] =>
          Context.current match {
            case Context.Total =>
              boundary.break(ErrorMessage.CantSequenceWithoutFallibleContext)
            case ctx @ given Context.PossiblyFallible[f] =>
              Transformation.Wrapped(
                p.source,
                recurse(p.wrapped),
                p.calculateTpe,
                if p.isHoisted == Plan.IsHoisted.Yes then Transformation.IsHoisted.Yes else Transformation.IsHoisted.No
              )

          }

        case leaf: Plan.Leaf =>
          Transformation.fromLeaf(leaf)

        case Plan.ConfedUp(config, _) =>
          Context.current match {
            case Context.Total =>
              config match {
                case update: Configured.Update =>
                  ConfedUp(update)
                case _: Configured.Sequence =>
                  boundary.break(ErrorMessage.CantSequenceWithoutFallibleContext)
              }
            case Context.PossiblyFallible(mode, wrapperType) =>
              ConfedUp(config)
          }
      }

    boundary[Transformation[F] | ErrorMessage](recurse(transformation)) match {
      case transformation: Transformation[F] => Right(transformation)
      case error: ErrorMessage               => Left(error)
    }
  }

  def fromLeaf(leaf: Plan.Leaf): Transformation.Leaf = Transformation.Leaf(leaf.output)

  enum Field[+F <: Fallible] derives Debug {
    case FromSource(name: String, transformation: Transformation[F])
    case FromModifier(modifier: Configured.NamedSpecific)

  }

  object Merged {
    enum Field[+F <: Fallible] derives Debug {
      case FromPrimary(source: Structure.Named, underlying: Transformation.Field[F])
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        transformation: Leaf | Merged[F]
      )
    }
  }
}
