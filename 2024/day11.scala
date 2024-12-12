package `2024`.day11

import prelude.*
import scala.annotation.tailrec

val next: Long => List[Long] = {
  case 0                    => List(1)
  case i if i.digits.isEven => (i divmod (10 ** (i.digits / 2))).toList
  case i                    => List(i * 2024)
}

type Stones = Map[Long, Long]

@tailrec def steps(m: Stones, n: Long): Stones =
  if n == 0 then m
  else
    val nextM = m.view
      .flatMap { (k, count) => next(k).map(_ -> count) }
      .groupMapReduce(_._1)(_._2)(_ + _)
    steps(nextM, n - 1)

@main def main() =
  val input = readInput(this).mkString.trim.split(" ").map(_.toLong).toList
  val stones = input.frequenciesL

  val part1 = steps(stones, 25)
  time {
    println(part1.values.sum)
    println(part1.size)
  }
  time {
    val part2 = steps(part1, 50)
    println(part2.values.sum)
    println(part2.size)
  }
