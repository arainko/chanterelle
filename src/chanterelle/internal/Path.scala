package chanterelle.internal

import scala.quoted.*
import scala.reflect.TypeTest

private[chanterelle] final case class Path(root: Type[?], segments: Vector[Path.Segment]) { self =>
  def appended(segment: Path.Segment): Path = self.copy(segments = segments.appended(segment))

  def prepended(segment: Path.Segment): Path = self.copy(segments = segments.prepended(segment))

  def narrowedCurrentTpe(using Quotes): Type[?] =
    segments.lastOption.fold(root)(_.tpe)

  // deliberately use something that requires a total function so that when a new Path.Segment is declared
  // it's not forgotten about
  def currentTpe(using Quotes): Type[?] = {
    segments.reverse.find {
      case Path.Segment.Element(_)         => true
      case Path.Segment.Field(_, _)        => true
      case Path.Segment.TupleElement(_, _) => true
    }
      .fold(root)(_.tpe)
      .repr
      .widen
      .asType
  }

  def toVector: Vector[Path.Segment] = self.segments

  def toList: List[Path.Segment] = self.segments.toList

  def isAncestorOrSiblingOf(that: Path)(using Quotes): Boolean = {
    /*
    import quotes.reflect.*

    if (self.segments.length > that.segments.length) false
    else
      self.root.repr =:= that.root.repr && self.segments.zip(that.segments).forall {
        case Path.Segment.Case(leftTpe) -> Path.Segment.Case(rightTpe) =>
          leftTpe.repr =:= rightTpe.repr
        case Path.Segment.Field(leftTpe, leftName) -> Path.Segment.Field(rightTpe, rightName) =>
          leftName == rightName && leftTpe.repr =:= rightTpe.repr
        case _ => false
      }
     */
    val thatRendered = that.render
    val thisRendered = this.render
    Logger.debug("that rendered", thatRendered)
    Logger.debug("this rendered", thisRendered)
    Logger.loggedDebug("Result")(thatRendered.contains(thisRendered))
  }

  def render(using Quotes): String = {
    if self.segments.isEmpty then "_"
    else
      self.segments.map {
        case Path.Segment.Field(_, name)    => name
        case Path.Segment.TupleElement(_, index) => s"apply($index)"
        case Path.Segment.Element(_)        => "element"
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
  }
}
