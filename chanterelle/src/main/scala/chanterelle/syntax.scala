package chanterelle

import chanterelle.hidden.TupleModifier
import chanterelle.internal.EntryPoint

import scala.NamedTuple.*

extension [Tup <: AnyNamedTuple](self: Tup) {

  /**
   * Transforms a named tuple with one of the supported modifiers
   *
   * {{{
   * val value = (field1 = 1, nestedField = (field = 2, optionalField = Some(3)))
   *
   * value.transform(
   *   _.put(_.nestedField)((newField = 4)), // creates a new field under 'nestedField'
   *   _.update(_.nestedField.optionalField.element)(_ + 1) // updates the value inside `optionalField` if it is a Some
   *   _.remove(_.field1) // completely removes 'field1'
   * )
   * // evaluates to: (nestedField = (field = 2, optionalField = Some(4), newField = 4))
   * // and is typed as: (nestedField : (field : Int, optionalField : Option[Int], newField : Int))
   * }}}
   *
   * @see chanterelle.hidden.TupledModifier.Builder
   */
  transparent inline def transform(inline modifications: TupleModifier.Builder[Tup] => TupleModifier[Tup]*) =
    EntryPoint.run[Tup](self, modifications*)
}

object test {

  val tup = (
    top1 = 1,
    top2 = 2,
    top3 = 
      (
        level1 = 1,
        level2 = 2,
        level3 = 3,
        level4 = 
          (
            low1 = 1,
            low2 = 2,
            low3 = 3
          )
      )
  )

  val mergee = (
    top1 = "1",
    top3 = 
      (
        level1 = "1", 
        level4 = (low4 = 4), 
        level5 = 123
      )
  )

  val expected =
    (
      top1 = mergee.top1,
      top2 = tup.top2,
      top3 = (
        level1 = mergee.top3.level1,
        level2 = tup.top3.level2,
        level3 = tup.top3.level3,
        level4 = (
          low1 = tup.top3.level4.low1,
          low2 = tup.top3.level4.low2,
          low3 = tup.top3.level4.low3,
          low4 = mergee.top3.level4.low4,
        ),
        level5 = mergee.top3.level5
      )
    )

    /*

    tup.transform(_.merge(mergee), _.merge(mergee2))
    */

  // case class Sources

}
