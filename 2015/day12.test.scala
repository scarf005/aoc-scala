import munit.FunSuite
import y2015.day12.*

class Day12Tests extends FunSuite:
  def check(fn: ujson.Value => Int, name: String)(xs: Vector[(String, Int)])(
    implicit loc: munit.Location,
  ): Unit =
    xs.foreach { (input, expected) =>
      test(s"$name: $input -> $expected"):
        assertEquals(fn(ujson.read(input)), expected)
    }

  check(part1, "part1") {
    Vector(
      "[1,2,3]" -> 6,
      """{"a":2,"b":4}""" -> 6,
      "[[[3]]]" -> 3,
      """{"a":{"b":4},"c":-1}""" -> 3,
      """{"a":[-1,1]}""" -> 0,
      """[-1,{"a":1}]""" -> 0,
      "[]" -> 0,
      "{}" -> 0,
    )
  }

  check(part2, "part2") {
    Vector(
      "[1,2,3]" -> 6,
      """[1,{"c":"red","b":2},3]""" -> 4,
      """{"d":"red","e":[1,2,3,4],"f":5}""" -> 0,
      """[1,"red",5]""" -> 6,
    )
  }
