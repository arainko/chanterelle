package chanterelle.internal

import chanterelle.*

object test {
  val tup = (camelCase = 1, camelCaseAgain = 2)

  tup.transform(_.rename(_.matchTyped[SnakeCaseConverter.ToSnakeCase]))
}
