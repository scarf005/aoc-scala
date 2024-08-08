package day2

import scala.util.chaining.*
import io.Source.fromFile
import day2.Day2.*

// The hell is a feet
// we use the metric system
// here thus the feet is redefined in meters
final case class Box(l: Int, w: Int, h: Int):
  def sides: Array[Int] = Array(l * w, w * h, h * l).map(_ * 2)
  def dimensions = Array(l, w, h)

object Box:
  def parse(s: String): Box = s.split("x").map(_.toInt) match
    case Array(l, w, h) => Box(l, w, h)

object Day2:
  def part1(input: Box): Int =
    val sides = input.pipe(_.sides)
    val smallest = sides.min

    sides.sum + smallest / 2

  def part2(input: Box): Int =
    val wrap = input.dimensions.sorted.take(2).sum * 2
    val bow = input.dimensions.product

    wrap + bow

@main def main() =
  val input = fromFile(".cache/02.txt").getLines.map(Box.parse).toArray

  println(input.map(part1).sum)
  println(input.map(part2).sum)
