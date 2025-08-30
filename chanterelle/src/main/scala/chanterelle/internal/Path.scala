package chanterelle.internal

import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] final case class Path(root: Type[?], segments: Vector[Path.Segment]) { self =>
  def appended(segment: Path.Segment): Path = self.copy(segments = segments.appended(segment))

  // def prepended(segment: Path.Segment): Path = self.copy(segments = segments.prepended(segment))

  // def narrowedCurrentTpe: Type[?] =
  // segments.lastOption.fold(root)(_.tpe)

  // deliberately use something that requires a total function so that when a new Path.Segment is declared
  // it's not forgotten about
  // def currentTpe(using Quotes): Type[?] = {
  //   segments.reverse.find {
  //     case Path.Segment.Element(_)         => true
  //     case Path.Segment.Field(_, _)        => true
  //     case Path.Segment.TupleElement(_, _) => true
  //   }
  //     .fold(root)(_.tpe)
  //     .repr
  //     .widen
  //     .asType
  // }

  // def toVector: Vector[Path.Segment] = self.segments

  def toList: List[Path.Segment] = self.segments.toList

  def stripLast: Option[(path: Path, last: Path.Segment)] =
    segments.lastOption match {
      case Some(last) => Some(this.copy(root, segments.dropRight(1)) -> last)
      case None       => None
    }

  // def isAncestorOrSiblingOf(that: Path)(using Quotes): Boolean = {
  //   val thatRendered = that.render
  //   val thisRendered = this.render
  //   Logger.debug("that rendered", thatRendered)
  //   Logger.debug("this rendered", thisRendered)
  //   Logger.loggedDebug("Result")(thatRendered.contains(thisRendered))
  // }

  def render: String = {
    if self.segments.isEmpty then "_"
    else
      self.segments.map {
        case Path.Segment.Field(_, name)         => name
        case Path.Segment.TupleElement(_, index) => s"apply($index)"
        case Path.Segment.Element(_)             => "element"
        case Path.Segment.LeftElement(_)         => "leftElement"
        case Path.Segment.RightElement(_)        => "rightElement"
      }.mkString(s"_.", ".", "")
  }
}

private[chanterelle] object Path {
  def empty(root: Type[?]): Path = Path(root, Vector.empty)

  given debug: Debug[Path] with {
    def astify(self: Path)(using Quotes): Debug.AST = Debug.AST.Text(self.render)
  }

  enum Segment derives Debug {
    def tpe: Type[?]

    final def narrow[A <: Segment](using tt: TypeTest[Segment, A]): Option[A] = tt.unapply(this)

    case Field(tpe: Type[?], name: String)
    case TupleElement(tpe: Type[?], index: Int)
    case Element(tpe: Type[?])
    case LeftElement(tpe: Type[?])
    case RightElement(tpe: Type[?])
  }
}
