package `2024`.day07

import prelude.*
import scala.annotation.tailrec

extension (a: Long)
  inline def ||(b: Long) = (a * math.pow(10, b.digits)).toLong + b

type Op = Long => Long => List[Long]

def getCombo(xs: List[Long], next: Op) =
  @tailrec def go(xs: List[Long], acc: List[Long] = List()): List[Long] =
    (xs, acc) match
      case (Nil, _)       => acc
      case (y :: ys, Nil) => go(ys, List(y))
      case (y :: ys, _)   => go(ys, acc.flatMap(next(y)))
  go(xs)

def solve(xss: Vector[(Long, List[Long])], op: Op) =
  xss.filter((target, xs) => getCombo(xs, op).exists(_ == target)).map(_._1).sum

def parse(input: String) = input.linesIterator.collect { case s"$target: $xs" =>
  (target.toLong, xs.split(" ").map(_.toLong).toList)
}.toVector

def part1(y: Long)(x: Long) = List(x + y, x * y)
def part2(y: Long)(x: Long) = List(x + y, x * y, x || y)

@main def main() =
  val input = readInput(this).mkString |> parse

  println(solve(input, part1))
  println(solve(input, part2))
