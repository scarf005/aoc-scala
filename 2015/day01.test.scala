import munit.FunSuite
import `2015`.day01.{part1, part2}

class Day1Tests extends FunSuite:
  test("Part1"):
    assertEquals(part1("(())"), 0)
    assertEquals(part1("()()"), 0)
    assertEquals(part1("((("), 3)
    assertEquals(part1("(()(()("), 3)
    assertEquals(part1("))((((("), 3)
    assertEquals(part1("())"), -1)
    assertEquals(part1("))("), -1)
    assertEquals(part1(")))"), -3)
    assertEquals(part1(")())())"), -3)

  test("Part2"):
    assertEquals(part2(")"), 1)
    assertEquals(part2("()())"), 5)
