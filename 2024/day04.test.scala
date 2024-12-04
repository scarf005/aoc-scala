package `2024`.day04

import munit.FunSuite

val example =
  """|MMMSXXMASM
     |MSAMXMSMSA
     |AMXSXMAAMM
     |MSAMASMSMX
     |XMASAMXAMM
     |XXAMMXXAMA
     |SMSMSASXSS
     |SAXAMASAAA
     |MAMMMXMMMM
     |MXMXAXMASX""".stripMargin.linesIterator.map(_.toVector).toVector

class Test extends FunSuite:
  test("part 1"):
    assertEquals(part1(example), 18)

  test("part 2"):
    assertEquals(part2(example), 9)
