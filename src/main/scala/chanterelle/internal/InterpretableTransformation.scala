package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap
import scala.collection.immutable.SortedMap
import scala.collection.Factory
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
        case t @ Transformation.Named(source, fields) =>
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
        case t @ Transformation.Tuple(source, fields) =>
          Tuple(source, fields.map((idx, t) => idx -> recurse(t)), t.calculateTpe)
        case t @ Transformation.Optional(source, paramTransformation) =>
          Optional(source, recurse(paramTransformation), t.calculateTpe)
        case t @ Transformation.Map(source, key, value) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type map[k, v]; map], '[collection.Map[key, value]]) => 
              Expr.summon[Factory[(key, value), map[key, value]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(source.tycon, Type.of[(key, value)])))
          }

          Map(source, recurse(key), recurse(value), factory, tpe)
        case t @ Transformation.Iter(source, elem) =>
          val tpe = t.calculateTpe
          val factory = ((source.tycon, tpe): @unchecked) match {
            case ('[type coll[a]; coll], '[Iterable[elem]]) =>
              Expr.summon[Factory[elem, coll[elem]]].getOrElse(boundary.break(ErrorMessage.NoFactoryFound(source.tycon, tpe)))
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
