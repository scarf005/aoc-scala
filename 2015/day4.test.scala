import munit.FunSuite
import y2015.day4.{part1, part2}

// Too slow
@munit.IgnoreSuite
class Day4Tests extends FunSuite:
  test("Part1"):
    assertEquals(part1("abcdef"), 609043)
    assertEquals(part1("pqrstuv"), 1048970)
