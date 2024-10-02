package `2022`.day04

import prelude.*

import munit.FunSuite

class Tests extends FunSuite:
  test("range"):
    assert((1 to 10) intersects (2 to 4))

  val input = """
    2-4,6-8
    2-3,4-5
    5-7,7-9
    2-8,3-7
    6-6,4-6
    2-6,4-8
  """.trim.split("\n").map(_.trim).flatMap(parse)

  test("Part 1"):
    assertEquals(part1(input), 2)

  // test("Part 2"):
  //   assertEquals(part2(res), 57600000)
