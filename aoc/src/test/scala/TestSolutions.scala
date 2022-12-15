import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(72602)(actual = Day01.answer1)
    assertResult(207410)(actual = Day01.answer2)
  }

  test("Day02") {
    assertResult(5)(actual = Day02.answer1)
    assertResult(5)(actual = Day02.answer2)
  }

  test("Day03") {
    assertResult(7766)(actual = Day02.answer1)
    assertResult(2415)(actual = Day02.answer2)
  }