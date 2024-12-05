package `2024`.day05

import prelude.*

def middle[A](xs: IndexedSeq[A]) = xs(xs.size / 2)

def parseOrd(input: Iterable[String]): Ordering[Int] = input
  .collect { case s"$a|$b" => a.toInt -> b.toInt }
  .groupMap(_._1)(_._2)
  .view
  .mapValues(_.toSet)
  .toMap
  .withDefaultValue(Set())
  |> (deps => Ordering.fromLessThan(deps(_)(_)))

def parsePages(input: Iterable[String]): Vector[Vector[Int]] =
  input.map(_.split(",").map(_.toInt).toVector).toVector

def parse(input: String): (Ordering[Int], Vector[Vector[Int]]) =
  input.split("\n\n").map(_.split("\n")) match {
    case Array(a, b) => (parseOrd(a), parsePages(b))
  }

extension (xss: Iterable[IndexedSeq[Int]])
  def partitionBySort(using deps: Ordering[Int]) = xss.partitionMap { xs =>
    val sorted = xs.sorted
    if xs == sorted then Right(sorted) else Left(sorted)
  }

@main def main() =
  val (deps, pages) = readInput(this).mkString |> parse
  val (wrongs, rights) = pages.partitionBySort(using deps)
  println(rights.sumBy(middle))
  println(wrongs.sumBy(middle))
