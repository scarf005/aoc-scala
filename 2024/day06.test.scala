package `2024`.day06

import prelude.*
import munit.FunSuite

val example =
  """|....#.....
     |.........#
     |..........
     |..#.......
     |.......#..
     |..........
     |.#..^.....
     |........#.
     |#.........
     |......#...""".stripMargin

class Test extends FunSuite:
  val (part1, part2) = example |> solve

  test("part 1"):
    assertEquals(part1, 41)

  test("part 2"):
    assertEquals(part2, 6)
