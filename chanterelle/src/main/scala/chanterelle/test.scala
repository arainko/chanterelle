package chanterelle

import chanterelle.hidden.TupleModifier
import chanterelle.internal.EntryPoint
import scala.NamedTuple.*

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
    one = (arr = 1),
    top1 = "1",
    top3 = 
      (
        level1 = "1", 
        level4 = (low4 = 4), 
        level5 = 123
      )
  )

  val tup1 = (one = (arr = "asd", ru = 1), two = 2)


  val actual = tup1.transform(_.merge(mergee))

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
