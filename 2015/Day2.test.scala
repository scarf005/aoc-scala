import munit.FunSuite
import day2.Day2.*
import day2.Box.parse

class Day2Tests extends FunSuite {
  val a = parse("2x3x4")
  val b = parse("1x1x10")

  test("Part1") {
    assertEquals(part1(a), 58)
    assertEquals(part1(b), 43)
  }

  test("Part2") {
    assertEquals(part2(a), 34)
    assertEquals(part2(b), 14)
  }
}
