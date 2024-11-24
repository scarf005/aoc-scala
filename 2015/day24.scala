package `2015`.day24

import prelude.*

def quantumEntanglement(xs: Iterable[Long]): Long = xs.product

def solution(input: Vector[Long], group: Int) =
  val groupWeight = input.sum / group
  (1 to input.size).iterator
    .map { n => input.combinations(n).filter(_.sum == groupWeight) }
    .find(_.nonEmpty)
    .get
    .map(quantumEntanglement)
    .min

@main def main() =
  val input = readInput(this).getLines.map(_.toLong).toVector

  val part1 = solution(input, 3)
  println(part1)
  val part2 = solution(input, 4)
  println(part2)
