package `2015`.day11

import munit.FunSuite

class Day11Tests extends FunSuite:
  test("Part 1"):
    assertEquals(solution("abcdefgh"), "abcdffaa")
    assertEquals(solution("ghijklmn"), "ghjaabcc")
