package day1

import scala.util.chaining.*
import io.Source.fromFile

def parse(s: String) = s.map { case '(' => 1; case ')' => -1 }

/** get final position of Santa */
def part1(input: String) = input.pipe(parse).sum

/** get position of first time Santa enters the basement */
def part2(input: String) = input.pipe(parse).scanLeft(0) { _ + _ }.indexOf(-1)

@main def main() =
  val input = fromFile(".cache/01.txt").mkString
  println(part1(input))
  println(part2(input))
