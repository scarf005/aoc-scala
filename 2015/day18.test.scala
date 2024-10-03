package `2015`.day18

import prelude.*
import munit.FunSuite

class Tests extends FunSuite:
  test("part1"):
    val grid = gol"""
      |.#.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####..
      """
    val steps = Iterator
      .iterate(grid)(_.part1Next)
      .take(5)
      .drop(1)
      .toSeq

    val expected = Seq(
      gol"""
      |..##..
      |..##.#
      |...##.
      |......
      |#.....
      |#.##..
      """,
      gol"""
      |..###.
      |......
      |..###.
      |......
      |.#....
      |.#....
      """,
      gol"""
      |...#..
      |......
      |...#..
      |..##..
      |......
      |......
      """,
      gol"""
      |......
      |......
      |..##..
      |..##..
      |......
      |......
      """,
    )
    assertEquals(steps, expected)

  test("part2"):
    val grid = gol"""
      |##.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####.#
      """
    val steps = Iterator
      .iterate(grid)(_.part2Next)
      .take(6)
      .drop(1)
      .toSeq

    val expected = Seq(
      gol"""
      |#.##.#
      |####.#
      |...##.
      |......
      |#...#.
      |#.####
      """,
      gol"""
      |#..#.#
      |#....#
      |.#.##.
      |...##.
      |.#..##
      |##.###
      """,
      gol"""
      |#...##
      |####.#
      |..##.#
      |......
      |##....
      |####.#
      """,
      gol"""
      |#.####
      |#....#
      |...#..
      |.##...
      |#.....
      |#.#..#
      """,
      gol"""
      |##.###
      |.##..#
      |.##...
      |.##...
      |#.#...
      |##...#
      """,
    )
    assertEquals(steps, expected)
