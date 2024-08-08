package day1

import scala.util.chaining.*
import io.Source.fromFile
import day1.Day1.*

def parse(s: String) = s.map { case '(' => 1; case ')' => -1 }

object Day1:
  /** get final height of Santa */
  def part1(input: String): Int =
    input.pipe(parse).sum

  /** get position of first time Santa enters the basement */
  def part2(input: String): Int =
    input.pipe(parse).scanLeft(0) { _ + _ }.indexOf(-1)

@main def main() =
  val input = fromFile(".cache/01.txt").mkString
  println(part1(input))
  println(part2(input))
