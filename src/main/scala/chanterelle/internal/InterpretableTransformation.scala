package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap
import scala.collection.immutable.SortedMap

enum InterpretableTransformation derives Debug {
  case Named(
    source: Structure.Named,
    fields: VectorMap[String, InterpretableTransformation.OfField],
    namesTpe: Type[? <: scala.Tuple],
    valuesTpe: Type[? <: scala.Tuple],
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
    outputTpe: Type[?]
  )

  case Iter[F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.Iter[F],
    elem: InterpretableTransformation,
    outputTpe: Type[?]
  )

  case Leaf(output: Structure.Leaf)

  case ConfedUp(config: Configured)
}

object InterpretableTransformation {

  def create(transformation: ModifiableTransformation[Nothing])(using Quotes): InterpretableTransformation =
    transformation match
      case t @ ModifiableTransformation.Named(source, fields) =>
        Named(
          source,
          fields.map {
            case (name, ModifiableTransformation.OfField.FromSource(_, t, removed)) =>
              name -> OfField.FromSource(name, create(t))
            case (name, ModifiableTransformation.OfField.FromModifier(mod, removed)) =>
              name -> OfField.FromModifier(mod)
          },
          t.calculateNamesTpe,
          t.calculateValuesTpe,
        )
      case t @ ModifiableTransformation.Tuple(source, fields) =>
        Tuple(source, fields.map((idx, t) => idx -> create(t)), t.calculateTpe)
      case t @ ModifiableTransformation.Optional(source, paramTransformation) =>
        Optional(source, create(paramTransformation), t.calculateTpe)
      case t @ ModifiableTransformation.Map(source, key, value) =>
        Map(source, create(key), create(value), t.calculateTpe)
      case t @ ModifiableTransformation.Iter(source, elem) =>
        Iter(source, create(elem), t.calculateTpe)
      case ModifiableTransformation.Leaf(output) =>
        Leaf(output)
      case ModifiableTransformation.ConfedUp(config) =>
        ConfedUp(config)

  enum OfField derives Debug {
    case FromSource(name: String, transformation: InterpretableTransformation)
    case FromModifier(modifier: Configured.NamedSpecific)
  }

}
