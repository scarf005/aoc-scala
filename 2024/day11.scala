package `2024`.day11

import prelude.*
import scala.annotation.tailrec

def memoize[A, B](f: A => B): A => B =
  val cache = collection.mutable.HashMap.empty[A, B]
  a => cache.getOrElseUpdate(a, f(a))

lazy val next: Long => List[Long] = memoize {
  case 0                    => List(1)
  case i if i.digits.isEven => (i divmod (10 ** (i.digits / 2))).toList
  case i                    => List(i * 2024)
}

inline def steps(m: Map[Long, Long], n: Long) =
  @tailrec def go(m: Map[Long, Long], n: Long): Map[Long, Long] =
    if n == 0 then m
    else {
      val nextM = m.view
        .flatMap { (k, count) => next(k).map(_ -> count) }
        .groupMapReduce(_._1)(_._2)(_ + _)
      go(nextM, n - 1)
    }

  go(m, n)

inline def solve(input: Map[Long, Long])(n: Long) = steps(input, n)

@main def main() =
  val input = readInput(this).mkString.trim.split(" ").map(_.toLong).toList
  val stones = input.frequenciesL

  time {
    val part1 = steps(stones, 25)
    println(part1.values.sum)
    println(part1.size)
  }

  time {
    val part2 = steps(stones, 75)
    println(part2.values.sum)
    println(part2.size)
  }
