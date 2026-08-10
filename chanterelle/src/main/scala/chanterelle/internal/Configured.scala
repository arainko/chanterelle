package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.SortedMap

private[chanterelle] enum Configured derives Debug {
  def tpe: Type[?]

  case Update(
    tpe: Type[?],
    fn: Sources.Ref
  )

  case Sequence(
    tpe: Type[?]
  )
}

private[chanterelle] object Configured {

  object Sequence {
    def fromTuple(plan: Plan.Tuple[Err], modifier: Modifier.Sequence): Either[ErrorMessage, Configured.Sequence] = {
      val (other, unwrappeds) = plan.fields.partitionMap {
        case (idx, p: Plan.Wrapped[Err, f]) => Right(idx -> p.wrapped)
        case (idx, other)                   => Left(idx -> other)
      }
      //
      // val updated = plan.updateAll {
      //   case Plan.Wrapped(wrapped = w) => w
      //   case other                     => other
      // }.cal
      ???
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
