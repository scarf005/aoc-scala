package `2024`.day02

import prelude.*

def safe(xs: Seq[Int]) =
  val isDesc = xs(0) > xs(1)
  xs.sliding(2).forall { case Seq(a, b) =>
    (if isDesc then a > b else a < b) && (1 to 3).contains((a - b).abs)
  }

def withoutOnes[A](xs: Seq[A]) =
  xs.zipWithIndex.map((_, i) => xs.patch(i, Nil, 1))

def part1(xss: Seq[Seq[Int]]): Int = xss.count(safe)

def part2(xss: Seq[Seq[Int]]): Int =
  xss.count(xs => safe(xs) || withoutOnes(xs).exists(safe))

@main def main() =
  val input =
    readInput(this).getLines.map(_.split(" ").map(_.toInt).toVector).toVector

  println(part1(input))
  println(part2(input))
