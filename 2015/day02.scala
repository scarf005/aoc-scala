package `2015`.day02

import scala.io.BufferedSource

import prelude.*

// The hell is a feet
// we use the metric system
// here thus the feet is redefined in meters
final case class Box(l: Int, w: Int, h: Int):
  def sides = List(l * w, w * h, h * l).map(_ * 2)
  def dimensions = List(l, w, h)

object Box:
  def parse(s: String): Box = s.split("x").map(_.toInt) match
    case Array(l, w, h) => Box(l, w, h)

/** surface area of the box + the area of the smallest side */
def part1(input: Box): Int = input.sides.sum + input.sides.min / 2

/** perimeter of the smallest face + volume of the box */
def part2(input: Box): Int =
  (input.dimensions.sorted.take(2).sum * 2) + input.dimensions.product

@main def main() =
  val input = fromFile(".cache/2015/02.txt").getLines.map(Box.parse).toArray

  println(input.map(part1).sum)
  println(input.map(part2).sum)
