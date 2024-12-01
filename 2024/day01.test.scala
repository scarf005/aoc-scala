package `2024`.day01

import munit.FunSuite

class Tests extends FunSuite:
  val example = Vector(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3",
  )

  val (lefts, rights) = example.flatMap(parse).unzip

  test("part 1"):
    assertEquals(part1(lefts, rights), 11)

  test("part 2"):
    assertEquals(part2(lefts, rights), 31)
