package `2024`.day02

import munit.FunSuite

val example = Vector(
  Vector(7, 6, 4, 2, 1),
  Vector(1, 2, 7, 8, 9),
  Vector(9, 7, 6, 2, 1),
  Vector(1, 3, 2, 4, 5),
  Vector(8, 6, 4, 4, 1),
  Vector(1, 3, 6, 7, 9),
)

class Test extends FunSuite:
  test("part 1"):
    assertEquals(safe(Vector(7, 6, 4, 2, 1)), true) // decreasing by 1 or 2
    assertEquals(safe(Vector(1, 2, 7, 8, 9)), false) // increase of 5
    assertEquals(safe(Vector(9, 7, 6, 2, 1)), false) // decrease of 4
    assertEquals(safe(Vector(1, 3, 2, 4, 5)), false) // not monotonic
    assertEquals(safe(Vector(8, 6, 4, 4, 1)), false) // no change
    assertEquals(safe(Vector(1, 3, 6, 7, 9)), true) // increasing by 1-3

  test("part 1"):
    assertEquals(part1(example), 2)

  test("part 2"):
    assertEquals(part2(example), 4)
