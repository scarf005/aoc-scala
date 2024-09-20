import munit.FunSuite
import day14.*

class Day14Tests extends FunSuite:
  val deers = Map(
    "Comet" -> Deer(14, 10, 127),
    "Dancer" -> Deer(16, 11, 162),
  )
  test("Part 1"):
    assertEquals(part1(deers.values, time = 1000), 1120)

  test("Part 2"):
    assertEquals(part2(deers, time = 1000), 689)
