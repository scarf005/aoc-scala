package y2015.day9

import prelude.*
import prelude.parser.*

import cats.parse.Parser

import y2015.day7.Parser.keyword

val city = alphas

val path = for
  from <- city <* keyword("to")
  to <- city <* keyword("=")
  score <- number
yield Set(from, to) -> score

@main def main() =
  val input =
    fromFile(".cache/2015/09.txt").getLines
      .map(_.trim) // input lines have trailing whitespaces
      .flatMap(path.parseAll)
      .toVector

  val scores = input.toMap
  val places = scores.keys.flatten
  def pathLen(path: Vector[String]) =
    path.sliding(2).map(_.toSet).map(scores).sum
  val lengths = places.toVector.permutations.map(pathLen).toVector

  println(s"part1 = ${lengths.min}")
  println(s"part2 = ${lengths.max}")
