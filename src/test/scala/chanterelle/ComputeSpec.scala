package chanterelle

import munit.FunSuite

// .compute needs its own file because Metals absolutely dies when referring to a value inside the compute lambda FOR SOME REASON
class ComputeSpec extends FunSuite {
  test(".compute puts a new field into a named tuple") {
    val tup = (anotherField = (field1 = 123))
    val expected = (anotherField = (field1 = 123, newField = 123 + 23))
    val actual = tup.transform(_.compute(_.anotherField)(value => (newField = value.field1 + 23)))

    assertEquals(actual, expected)
  }
}
