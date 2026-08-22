package chanterelle.internal

import scala.util.boundary
import chanterelle.internal.Transformation.Field
import chanterelle.internal.Transformation.Merged
import scala.annotation.publicInBinary

private[chanterelle] object FallibilityRefiner {
  def run(transformation: Transformation[Fallible]): Transformation[Nothing] | None =
    recurse(transformation) match
      case ()   => transformation.asInstanceOf[Transformation[Nothing]]
      case None => None

  @publicInBinary
  private[FallibilityRefiner] def recurse(trans: Transformation[Fallible]): Unit | None =
    boundary[Unit | None] {
      trans match
        case Transformation.Named(source, fields, namesTpe, valuesTpe, _) =>
          evaluate(fields.collect { case (_, Field.FromSource(name, transformation)) => transformation })
        case Transformation.Tuple(source, fields, outputTpe) =>
          evaluate(fields.values)
        case Transformation.Optional(source, paramTransformation, outputTpe) =>
          recurse(paramTransformation)
        case Transformation.EitherLike(source, left, right, outputTpe) =>
          recurse(left)
          recurse(right)
        case Transformation.MapLike(source, key, value, factory, outputTpe) =>
          recurse(key)
          recurse(value)
        case Transformation.IterLike(source, elem, factory, outputTpe) =>
          recurse(elem)
        case Transformation.Leaf(output) =>
          ()
        case Transformation.ConfedUp(config) =>
          config match
            case Configured.Update(tpe, fn)                      => ()
            case Configured.Sequence(tpe, source, unwrappedDest) => boundary.break(None)

        case Transformation.Merged(mergees, fields, namesTpe, valuesTpe, _) =>
          evaluate(fields.collect {
            case (_, Merged.Field.FromPrimary(underlying = Field.FromSource(transformation = t))) => t
            case (_, Merged.Field.FromSecondary(transformation = t))                              => t
          })
        case Transformation.Wrapped(source, wrapped, outputTpe, isHoisted) =>
          boundary.break(None)

    }

  private inline def evaluate(plans: Iterable[Transformation[Fallible]])(using inline label: boundary.Label[None | Unit]) =
    val iterator = plans.iterator
    while iterator.hasNext do
      recurse(iterator.next()) match {
        case None => boundary.break(None)
        case ()   => ()
      }

}
