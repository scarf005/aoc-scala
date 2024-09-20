import munit.FunSuite
import day11.*

class Day11Tests extends FunSuite:
  test("Part 1"):
    assertEquals(solution("abcdefgh"), "abcdffaa")
    assertEquals(solution("ghijklmn"), "ghjaabcc")
