package chanterelle.internal

import scala.quoted.*

private[chanterelle] enum Configured[+F <: Fallible] derives Debug {
  def tpe: Type[?]

  case Update(
    tpe: Type[?],
    fn: Sources.Ref
  ) extends Configured[Nothing]
}

private[chanterelle] object Configured {

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
