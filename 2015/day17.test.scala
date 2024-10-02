package `2015`.day17

import prelude.*
import munit.FunSuite
import `2015`.day17.*


class Tests extends FunSuite:
  // given Logger = PrintLogger
  test("part 1"):
    val res = part1(target = 25, containers = Seq(20, 15, 10, 5, 5))
    assertEquals(res, 4)

  test("part2"):
    val res = part2(target = 25, containers = Seq(20, 15, 10, 5, 5))
    assertEquals(res, 3)
