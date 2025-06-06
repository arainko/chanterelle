package chanterelle.internal

import NamedTuple.AnyNamedTuple
import scala.quoted.*

enum Modifier {
  case Add(path: Path, value: Expr[? <: AnyNamedTuple])
  case Compute(path: Path, function: Expr[? <: AnyNamedTuple => ? <: AnyNamedTuple])
  
}
