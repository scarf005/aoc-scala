package day9

import prelude.*

import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Rfc5234.alpha

import day6.number
import day7.Parser.keyword
import utils.either2Iterable

val city: Parser[String] = alpha.rep.string

val path = for
  from <- city <* keyword("to")
  to <- city <* keyword("=")
  score <- number
yield Set(from, to) -> score

@main def main() =
  val input =
    fromFile(".cache/09.txt").getLines
      .map(_.trim) // input lines have trailing whitespaces
      .flatMap(path.parseAll)
      .toVector

  val scores = input.toMap
  val places = scores.keys.flatten
  def pathLen(path: Seq[String]) = path.sliding(2).map(_.toSet).map(scores).sum
  val lengths = places.toSeq.permutations.map(pathLen).toSeq

  println(s"part1 = ${lengths.min}")
  println(s"part2 = ${lengths.max}")
