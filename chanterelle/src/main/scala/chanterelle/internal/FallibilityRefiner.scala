// package chanterelle.internal
//
// import scala.util.boundary
// import chanterelle.internal.Transformation.Field
// import chanterelle.internal.Transformation.Merged
// import chanterelle.internal.Plan.IsHoisted
// import scala.annotation.publicInBinary
//
// private[chanterelle] object FallibilityRefiner {
//   def run(transformation: Transformation): transformation.type | None = 
//     recurse(transformation) match
//       case () => transformation 
//       case None => None
//
//   @publicInBinary
//   private[FallibilityRefiner] def recurse(trans: Transformation): Unit | None =
//     boundary[Unit | None] {
//       trans match
//         case Transformation.Named(source, fields, namesTpe, valuesTpe) =>
//           evaluate(fields.collect { case (_, Field.FromSource(name, transformation)) => transformation })
//         case Transformation.Tuple(source, fields, outputTpe) =>
//           evaluate(fields.values)
//         case Transformation.Optional(source, paramTransformation, outputTpe) =>
//           recurse(paramTransformation)
//         case Transformation.EitherLike(source, left, right, outputTpe) =>
//           recurse(left)
//           recurse(right)
//         case Transformation.MapLike(source, key, value, factory, outputTpe) =>
//           recurse(key)
//           recurse(value)
//         case Transformation.IterLike(source, elem, factory, outputTpe) =>
//           recurse(elem)
//         case Transformation.Leaf(output) =>
//           ()
//         case Transformation.ConfedUp(config) =>
//           config match
//             case Configured.Update(tpe, fn)                      => ()
//             case Configured.Sequence(tpe, source, unwrappedDest) => None
//
//         case Transformation.Merged(mergees, fields, namesTpe, valuesTpe) =>
//           evaluate(fields.collect { case (_, Merged.Field.FromPrimary(underlying = Field.FromSource(transformation = t))) => t })
//         case Transformation.Wrapped(source, wrapped, outputTpe, isHoisted) =>
//           evaluate(wrapped :: Nil)
//           if isHoisted == IsHoisted.Yes then None else ()
//
//     }
//
//   private inline def evaluate(plans: Iterable[Transformation])(using inline label: boundary.Label[None | Unit]) =
//     val iterator = plans.iterator
//     while iterator.hasNext do
//       recurse(iterator.next()) match {
//         case None => boundary.break(None)
//         case ()   => ()
//       }
//
// }
