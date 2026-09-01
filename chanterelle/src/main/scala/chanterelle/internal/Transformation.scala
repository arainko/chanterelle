package chanterelle.internal

import chanterelle.internal.Plan.IsModified

import scala.collection.Factory
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import scala.util.boundary
import scala.util.boundary.Label
import chanterelle.internal.Plan.Hoist
import chanterelle.internal.Context.Total
import chanterelle.internal.Context.PossiblyFallible
import chanterelle.Mode
import chanterelle.internal.FallibleInterpreter.TransformationMode

private[chanterelle] sealed trait Transformation[+F <: Fallible] derives Debug {

  def outputTpe: Type[?]
}

object Transformation {
  case class Named[+F <: Fallible](
    source: Structure.Named,
    fields: VectorMap[String, Transformation.Field[F]],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple],
    outputTpe: Type[? <: NamedTuple.AnyNamedTuple]
  ) extends Transformation[F]

  case class Tuple[+F <: Fallible](
    source: Structure.Tuple,
    fields: SortedMap[Int, Transformation[F]],
    outputTpe: Type[?]
  ) extends Transformation[F]

  case class Optional[+F <: Fallible](
    source: Structure.Optional,
    paramTransformation: Transformation[F],
    outputTpe: Type[? <: Option[?]]
  ) extends Transformation[F]

  case class EitherLike[+F <: Fallible](
    source: Structure.Either,
    left: Transformation[Nothing],
    right: Transformation[F],
    outputTpe: Type[? <: scala.Either[?, ?]]
  ) extends Transformation[F]

  case class MapLike[+F <: Fallible, map[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[map],
    key: Transformation[F],
    value: Transformation[F],
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  ) extends Transformation[F]

  case class IterLike[+F <: Fallible, iter[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[iter],
    elem: Transformation[F],
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  ) extends Transformation[F]

  case class Leaf(output: Structure.Leaf) extends Transformation[Nothing] {
    export output.tpe as outputTpe
  }

  case class ConfedUp[+F <: Fallible](config: Configured[F]) extends Transformation[F] {
    export config.tpe as outputTpe
  }

  case class Merged(
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, Transformation.Merged.Field],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple],
    outputTpe: Type[? <: NamedTuple.AnyNamedTuple]
  ) extends Transformation[Nothing]

  // OK so, small invariant:
  // * hoisting a wrapped node hoists all encountered wrapped nodes on its way
  // * this means that if we encounter a transformation with IsHoisted == No we can be sure that __THERE ARE NO__ fallible nodes in 'wrapped'
  // * type enforcement should be: if IsHoisted == No then ElemTransformation.NonFallible, every other combo is possible when IsHoisted.Yes.
  // TODO: sooo given the above, isHoisted and wrapped form a 3 arm enum? NonHoisted(Transformation[Fallible]), Hoisted(NonFallible), Hoisted(Fallible)
  case class Hoisted[F[_]](
    source: Structure.Wrapped[F],
    wrapped: ElemTransformation,
    outputTpe: Type[?]
  ) extends Transformation[Fallible]

  enum ElemTransformation {
    case HoistedFallible[F[_]](transformation: Transformation[Fallible], mode: Expr[Mode.FailFast[F]])
    case HoistedNonFallible(transformation: Transformation[Nothing])
  }

  case class Mapped[F[_]](
    source: Structure.Wrapped[F],
    wrapped: Transformation[Nothing],
    mode: Expr[Mode[F]],
    outputTpe: Type[?]
  ) extends Transformation[Nothing]

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

    def fromMerged(
      plan: Plan.Merged[Nothing]
    )(using Quotes, Label[ErrorMessage], Context.Any): Transformation.Merged =
      Context.current.asTotal.locally {
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
        (plan.calculateNamesTpe, plan.calculateValuesTpe).runtimeChecked match {
          case '[type names <: scala.Tuple; names] -> '[type values <: scala.Tuple; values] =>
            Merged(
              plan.mergees,
              fields,
              Type.of[names],
              Type.of[values],
              Type.of[NamedTuple.NamedTuple[names, values]]
            )

        }

      }

    def recurse[F <: Fallible](transformation: Plan[Nothing])(using Label[ErrorMessage], Context.Of[F]): Transformation[F] =
      transformation match {
        // optimization: if a Transformation hasn't been modified it's valid to just treat it as a Leaf (i.e. rewrite the source value)
        // case plan if plan.isModified == IsModified.No =>
        //   val tpe = plan.calculateTpe
        //   Leaf(Structure.Leaf(tpe, Path.empty(tpe))) // TODO: figure out what to do about the path here
        //
        case p @ Plan.Named(source, fields, _) =>
          (p.calculateNamesTpe, p.calculateValuesTpe).runtimeChecked match {
            case '[type names <: scala.Tuple; names] -> '[type values <: scala.Tuple; values] =>
              Named(
                source,
                fields.transform((_, field) => transformField(field)),
                Type.of[names],
                Type.of[values],
                Type.of[NamedTuple.NamedTuple[names, values]]
              )

          }

        case p @ Plan.Tuple(source, fields, _) =>
          Tuple(source, fields.map((idx, plan) => idx -> recurse(plan)), p.calculateTpe)

        case p @ Plan.Optional(source, paramTransformation, _) =>
          Optional(source, recurse(paramTransformation), p.calculateTpe)

        case p @ Plan.Either(source, left, right, _) =>
          EitherLike(source, Context.current.asTotal.locally(recurse(left)), recurse(right), p.calculateTpe)

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
            case ctx @ given Context.PossiblyFallible[`f`] =>
              (p.isHoisted, ctx.mode) match
                case Plan.Hoist.Passthrough -> (TransformationMode.FailFast(mode)) =>
                  Transformation.Hoisted(
                    p.source,
                    ElemTransformation.HoistedFallible(recurse(p.wrapped), mode),
                    p.wrapped.calculateTpe
                  )

                case Plan.Hoist.Passthrough -> TransformationMode.Accumulating(_) =>
                  boundary.break(ErrorMessage.CantSequenceWithoutFallibleContext)

                case Plan.Hoist.Yes -> _ =>
                  Transformation.Hoisted(
                    p.source,
                    Context.current.asTotal.locally(ElemTransformation.HoistedNonFallible(recurse(p.wrapped))),
                    p.wrapped.calculateTpe
                  )

                case Plan.Hoist.No -> _ =>
                  Transformation.Mapped[f](
                    p.source,
                    Context.current.asTotal.locally(recurse(p.wrapped)),
                    ctx.mode.value,
                    p.calculateTpe
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
    enum Field derives Debug {
      case FromPrimary(source: Structure.Named, underlying: Transformation.Field[Nothing])
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        transformation: Leaf | Merged
      )
    }
  }
}
