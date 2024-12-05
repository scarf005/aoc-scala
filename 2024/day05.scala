package `2024`.day05

import prelude.*

def middle[A](xs: IndexedSeq[A]) = xs(xs.size / 2)

@main def main() =
  val (rawRules, rawPages) = readInput(this).mkString
    .split("\n\n")
    .map(_.split("\n")) match { case Array(a, b) => (a, b) }

  given ord: Ordering[Int] = rawRules
    .collect { case s"$a|$b" => a.toInt -> b.toInt }
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap
    .withDefaultValue(Set())
    |> (deps => Ordering.fromLessThan(deps(_)(_)))

  val pages = rawPages.map(_.split(",").map(_.toInt).toVector).toVector

  val (wrongs, rights) = pages.partitionMap { page =>
    val sort = page.sorted
    if page == sort then Right(sort) else Left(sort)
  }
  println(rights.sumBy(middle))
  println(wrongs.sumBy(middle))
