package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.SortedMap

private[chanterelle] enum Configured[+F <: Fallible] derives Debug {
  def tpe: Type[?]

  case Update(
    tpe: Type[?],
    fn: Sources.Ref
  ) extends Configured[Nothing]

  case Sequence(
    tpe: Type[?],
    source: Structure.Tuple,
    unwrappedDest: Type[?]
  ) extends Configured[Fallible]
}

private[chanterelle] object Configured {

  object Sequence {
    def fromTuple[F[_]](plan: Plan.Tuple[Err, Fallible], modifier: Modifier.Sequence[F])(using
      Quotes
    ): Either[ErrorMessage, Configured[Fallible]] = {
      val (other, unwrappeds) = plan.fields.partitionMap {
        case (idx, p: Plan.Wrapped[Err, f]) => Right(idx -> p.wrapped)
        case (idx, other)                   => Left(idx -> other)
      }
      //
      val updated = plan.updateAll {
        case Plan.Wrapped(wrapped = w) => w
        case other                     => other
      }.calculateTpe
      given Type[F] = modifier.wrapperType.wrapper
      val tpe = updated match { case '[tpe] => Type.of[F[tpe]] }
      if other.isEmpty then Right(Configured.Sequence(tpe, plan.source, updated)) else Left(???)
    }
  }

  sealed trait NamedSpecific derives Debug {
    def tpe: Type[?]
  }

  object NamedSpecific {
    case class Add(
      valueStructure: Structure.Named.Singular,
      value: Sources.Ref
    ) extends NamedSpecific {
      export valueStructure.fieldName
      export valueStructure.valueStructure.tpe
    }

    case class Compute(
      valueStructure: Structure.Named.Singular,
      fn: Sources.Ref
    ) extends NamedSpecific {
      export valueStructure.fieldName
      export valueStructure.valueStructure.tpe
    }
  }
}
