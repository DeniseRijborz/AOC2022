import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(72602)(actual = Day01.answer1)
    assertResult(207410)(actual = Day01.answer2)
  }

  test("Day02") {
    assertResult(10718)(actual = Day02.answer1)
    assertResult(14652)(actual = Day02.answer2)
  }

  test("Day03") {
    assertResult(7766)(actual = Day03.answer1)
    assertResult(2415)(actual = Day03.answer2)
  }

  test("Day04") {
    assertResult(536)(actual = Day04.answer1)
    assertResult(845)(actual = Day04.answer2)
  }