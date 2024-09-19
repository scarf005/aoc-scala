package day10

import prelude.*

val groupNums = raw"(.)\1*".r

def nextNums(s: String) = groupNums
  .findAllIn(s)
  .map { xs => s"${xs.length}${xs.head}" }
  .mkString

def solution(input: String)(n: Int) =
  (1 to n).foldLeft(input) { (acc, _) => nextNums(acc) }.length

@main def main() =
  val input = fromFile(".cache/10.txt").mkString.trim
  val solver = solution(input)

  val part1 = solver(40)
  println(part1)

  val part2 = solver(50)
  println(part2)
