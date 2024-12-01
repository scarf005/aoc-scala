package `2015`.day16

import prelude.*
import prelude.parser.*

import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Rfc5234.{digit, alpha}

type Clue = Map[String, Int]

val id: Parser[Int] =
  string("Sue ") *> number <* string(": ")

val clue: Parser[(String, Int)] =
  (alphas <* string(": ")) ~ number

val clues: Parser[Clue] =
  clue.repSep(string(", ")).map(_.toList.toMap)

val aunt: Parser[Aunt] =
  (id ~ clues).map { (id, clues) => Aunt(id, clues) }

case class Aunt(id: Int, info: Clue):
  def matchEq(known: Clue): Boolean =
    (info zipByKey known).values.forall { (a, b) => a == b }

  def matchRange(known: Clue): Boolean =
    (info zipByKey known).forall {
      case (k, v) if Set("cats", "trees") contains k           => v._1 > v._2
      case (k, v) if Set("pomeranians", "goldfish") contains k => v._1 < v._2
      case (k, v)                                              => v._1 == v._2
    }

val known = Map(
  "children" -> 3,
  "cats" -> 7,
  "samoyeds" -> 2,
  "pomeranians" -> 3,
  "akitas" -> 0,
  "vizslas" -> 0,
  "goldfish" -> 5,
  "trees" -> 3,
  "cars" -> 2,
  "perfumes" -> 1,
)

def part1(xs: Iterable[Aunt]) =
  xs.find(_.matchEq(known)).get.id

def part2(xs: Iterable[Aunt]) =
  xs.find(_.matchRange(known)).get.id

@main def main() =
  val input = readInput(this).getLines
    .map(_.trim)
    .flatMap(aunt.parseAll)
    .toSeq

  println(part1(input))
  println(part2(input))
