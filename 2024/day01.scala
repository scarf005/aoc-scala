package `2024`.day01

import prelude.*

def parse(s: String) = s lift { case s"$a   $b" => (a.toInt, b.toInt) }

def part1(xs: Vector[Int], ys: Vector[Int]) =
  (xs.sorted zip ys.sorted).sumBy((a, b) => (a - b).abs)

def part2(xs: Vector[Int], ys: Vector[Int]) =
  (xs.frequencies zipByKey ys.frequencies)
    .sumBy { case (k, (a, b)) => k * a * b }

@main def main() =
  val (lefts, rights) = readInput(this).getLines.toVector.flatMap(parse).unzip

  println(part1(lefts, rights))
  println(part2(lefts, rights))
