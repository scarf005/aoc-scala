package `2015`.day03

import munit.FunSuite

class Day3Tests extends FunSuite:
  test("Part1"):
    assertEquals(part1(">"), 2)
    assertEquals(part1("^>v<"), 4)
    assertEquals(part1("^v^v^v^v^v"), 2)

  test("Part2"):
    assertEquals(part2("^v"), 3)
    assertEquals(part2("^>v<"), 3)
    assertEquals(part2("^v^v^v^v^v"), 11)
