package `2015`.day04

import munit.FunSuite

// Too slow
@munit.IgnoreSuite
class Day4Tests extends FunSuite:
  test("Part1"):
    assertEquals(part1("abcdef"), 609043)
    assertEquals(part1("pqrstuv"), 1048970)
