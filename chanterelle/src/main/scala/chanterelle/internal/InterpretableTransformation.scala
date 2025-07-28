package chanterelle.internal

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

  case Map[F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.Map[F],
    key: InterpretableTransformation,
    value: InterpretableTransformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case Iter[F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.Iter[F],
    elem: InterpretableTransformation,
    factory: Expr[Factory[?, ?]],
    outputTpe: Type[?]
  )

  case Leaf(output: Structure.Leaf)

  case ConfedUp(config: Configured)
}

object InterpretableTransformation {

  def create(transformation: Transformation[Nothing])(using Quotes): Either[ErrorMessage, InterpretableTransformation] = {
    def recurse(transformation: Transformation[Nothing])(using Label[ErrorMessage]): InterpretableTransformation =
      transformation match {
        // optimization: if a Transformation hasn't been modified it's valid to just treat it as a Leaf (i.e. rewrite the source value)
        case t @ Transformation.IsNotModified() =>
          val tpe = t.calculateTpe
          Leaf(Structure.Leaf(tpe, Path.empty(tpe))) // TODO: figure out what to do about the path here
        // TODO: remove 'removed' from OfField
        case t @ Transformation.Named(source, fields, _) =>
          Named(
            source,
            fields.map {
              case (name, Transformation.OfField.FromSource(_, t, removed)) =>
                name -> OfField.FromSource(name, recurse(t))
              case (name, Transformation.OfField.FromModifier(mod, removed)) =>
                name -> OfField.FromModifier(mod)
            },
            t.calculateNamesTpe,
            t.calculateValuesTpe
          )
        case t @ Transformation.Tuple(source, fields, _) =>
          Tuple(source, fields.map((idx, t) => idx -> recurse(t)), t.calculateTpe)
        case t @ Transformation.Optional(source, paramTransformation, _) =>
          Optional(source, recurse(paramTransformation), t.calculateTpe)
        case t @ Transformation.Map(source, key, value, _) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type map[k, v]; map], '[collection.Map[key, value]]) =>
              Expr.summon[Factory[(key, value), map[key, value]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }

          Map(source, recurse(key), recurse(value), factory, tpe)
        case t @ Transformation.Iter(source, elem, _) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type coll[a]; coll], '[Iterable[elem]]) =>
              Expr.summon[Factory[elem, coll[elem]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(tpe)))
          }
          Iter(source, recurse(elem), factory, tpe)
        case Transformation.Leaf(output) =>
          Leaf(output)
        case Transformation.ConfedUp(config) =>
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
}
