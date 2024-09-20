import munit.FunSuite
import day10.*

class Day10Tests extends FunSuite:
  test("Part 1"):
    Seq("1", "11", "21", "1211", "111221", "312211")
      .sliding(2)
      .map { case Seq(a, b) => assertEquals(nextNums(a), b) }
