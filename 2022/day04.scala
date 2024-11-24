package `2022`.day04

import prelude.*

extension (a: Range)
  inline infix def includes(b: Range): Boolean =
    a.start <= b.start && b.end <= a.end

  inline infix def intersects(b: Range): Boolean =
    (a includes b) || (b includes a)

object Range:
  def unapply(s: String) =
    s lift { case s"$a-$b" => (a.toInt to b.toInt) }

def parse(s: String) =
  s lift { case s"${Range(a)},${Range(b)}" => (a, b) }

def part1(xs: Iterable[(Range, Range)]) =
  xs.map { (a, b) => (a intersects b).toInt }.sum

def part2(xs: Iterable[(Range, Range)]) =
  xs.map { (a, b) => (a intersect b).nonEmpty.toInt }.sum

@main def main() =
  val input = readInput(this).getLines.flatMap(parse).toSeq

  println(part1(input))
  println(part2(input))
