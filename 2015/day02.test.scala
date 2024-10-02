package `2015`.day02

import munit.FunSuite

class Day2Tests extends FunSuite:
  import Box.parse
  val a = parse("2x3x4")
  val b = parse("1x1x10")

  test("Part1"):
    assertEquals(part1(a), 58)
    assertEquals(part1(b), 43)

  test("Part2"):
    assertEquals(part2(a), 34)
    assertEquals(part2(b), 14)
