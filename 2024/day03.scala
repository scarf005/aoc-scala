package `2024`.day03

import prelude.*
import scala.util.matching.Regex

val mul = raw"mul\((\d+),(\d+)\)".r
val `do` = raw"do\(\)".r
val dont = raw"don't\(\)".r

def part1(text: String): Int =
  mul.findAllMatchIn(text).sumBy { case mul(a, b) => a.toInt * b.toInt }

def part2(text: String): Int = s"(${mul}|${`do`}|${dont})".r
  .findAllMatchIn(text)
  .foldLeft((true, 0)) { case ((on, acc), m) =>
    m match
      case mul(a, b) => (on, if on then acc + a.toInt * b.toInt else acc)
      case `do`()    => (true, acc)
      case dont()    => (false, acc)
  }
  ._2

@main def main() =
  val text = readInput(this).mkString

  println(part1(text))
  println(part2(text))
