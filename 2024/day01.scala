package `2024`.day01

import prelude.*

def parse(s: String) = s lift { case s"$a   $b" => (a.toInt, b.toInt) }

extension [A](xs: Iterable[A])
  def counts: Map[A, Int] =
    xs.groupMapReduce(identity)(_ => 1)(_ + _)

def part1(lefts: Vector[Int], rights: Vector[Int]) =
  (lefts.sorted zip rights.sorted).map((a, b) => (b - a).abs).sum

def part2(lefts: Vector[Int], rights: Vector[Int]) =
  (lefts.counts zipByKey rights.counts).sumBy { case (k, (a, b)) => k * a * b }

@main def main() =
  val (lefts, rights) = readInput(this).getLines.toVector.flatMap(parse).unzip

  println(part1(lefts, rights))
  println(part2(lefts, rights))
