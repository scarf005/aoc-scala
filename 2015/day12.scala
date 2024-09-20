package day12

import prelude.*

/** recursively sum all numbers in a JSON object */
def part1(json: ujson.Value): Int = json match
  case ujson.Obj(obj) => obj.values.map(part1).sum
  case ujson.Arr(arr) => arr.map(part1).sum
  case ujson.Num(n) => n.toInt
  case _            => 0

/** recursively sum all numbers in a JSON object that does not contain the value "red" */
def part2(json: ujson.Value): Int = json match
  case ujson.Obj(obj) if obj.values.exists(_ == ujson.Str("red")) => 0
  case ujson.Obj(obj) => obj.values.map(part2).sum
  case ujson.Arr(arr) => arr.map(part2).sum
  case ujson.Num(n) => n.toInt
  case _            => 0

@main def main =
  val input = fromFile(".cache/12.txt").mkString.pipe(ujson.read(_))

  println(part1(input))
  println(part2(input))
