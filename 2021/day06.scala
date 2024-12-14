package `2021`.day06

import scala.io.Source.fromFile
import scala.annotation.tailrec

type Fishes = Map[Long, Long]

def next(n: Long): List[Long] = n match
  case 0 => 6L :: 8L :: Nil
  case i => (i - 1) :: Nil

@tailrec def steps(fishes: Fishes, n: Long): Fishes =
  if n == 0 then fishes
  else
    val newFishes = fishes.view
      .flatMap { (k, count) => next(k).map(_ -> count) }
      .groupMapReduce(_._1)(_._2)(_ + _)
    steps(newFishes, n - 1)

def solve(fishes: Fishes, n: Long): Long = steps(fishes, n).values.sum

def part1(input: String): Long = solve(parse(input), 80)
def part2(input: String): Long = solve(parse(input), 256)

def parse(input: String): Fishes =
  input.trim.split("\n").map(_.toLong).groupMapReduce(identity)(_ => 1L)(_ + _)

@main def main() =
  val input = fromFile(".cache/2021/06.txt").mkString

  println(part1(input))
  println(part2(input))
