package chanterelle

import scala.quoted.*

sealed trait Selector {
  extension [A] (self: Option[A] | Iterable[A]) def element: A
}
