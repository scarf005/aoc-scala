import munit.FunSuite
import y2015.day10.*

class Day10Tests extends FunSuite:
  test("Part 1"):
    Vector("1", "11", "21", "1211", "111221", "312211")
      .sliding(2)
      .map { case Vector(a, b) => assertEquals(nextNums(a), b) }
