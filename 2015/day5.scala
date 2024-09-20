package day5

import prelude.*
import scala.util.matching.Regex

val naughtyRegex = "ab|cd|pq|xy".r

def part1(s: String): Boolean =
  val vowels = s.count("aeiou".contains)
  val doubles = s.sliding(2).exists { xs => xs(0) == xs(1) }
  val isNaughty = naughtyRegex.findFirstIn(s).isDefined

  vowels >= 3 && doubles && !isNaughty

val part2Regex = Vector(raw"(..).*\1".r, raw"(.).\1".r)

def part2(s: String): Boolean =
  def matches(x: Regex) = x.findFirstIn(s).isDefined

  part2Regex.forall(matches)

@main def main() =
  val input = fromFile(".cache/05.txt").getLines().toVector

  def solve(f: String => Boolean) = input.map(f).count(identity)

  println(solve(part1))
  println(solve(part2))
