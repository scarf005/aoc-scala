package `2024`.day07

import prelude.*
import munit.FunSuite

val example =
  """|190: 10 19
     |3267: 81 40 27
     |83: 17 5
     |156: 15 6
     |7290: 6 8 6 15
     |161011: 16 10 13
     |192: 17 8 14
     |21037: 9 7 18 13
     |292: 11 6 16 20""".stripMargin

class Test extends FunSuite:
  val input = example |> parse

  test("part 1"):
    assertEquals(solve(input, part1), 3749L)

  test("part 2"):
    assertEquals(solve(input, part2), 11387L)
