package day13

import scala.io.Source.fromFile
import scala.util.chaining.*

type Score = Map[String, Map[String, Int]]

def parse(input: String) = input match
  case s"$from would gain $n happiness units by sitting next to $to." =>
    from -> (to -> n.toInt)
  case s"$from would lose $n happiness units by sitting next to $to." =>
    from -> (to -> -n.toInt)

def parseToScore(xs: Iterable[String]): Score = xs
  .map(parse)
  .groupBy(_._1)
  .view
  .mapValues(_.map(_._2).toMap)
  .toMap

extension [A](xs: Seq[A])
  def slidingCyclic(size: Int) =
    xs.concat(xs).sliding(size).take(xs.size)

  def permutationsCyclic =
    xs.drop(1).permutations.map(xs.head +: _)

def score(comb: Seq[String])(implicit scores: Score) =
  comb
    .slidingCyclic(3)
    .collect { case Seq(left, center, right) =>
      val f = scores.getOrElse(center, Map.empty)
      f.getOrElse(left, 0) + f.getOrElse(right, 0)
    }
    .sum

def part1(scores: Score) =
  val people = scores.keys.toSeq
  val combination = people.permutationsCyclic.toSeq

  given Score = scores
  combination.map(score).max

def part2(scores: Score) =
  val people = scores.keys.toSeq :+ "Me"
  val combination = people.permutationsCyclic.toSeq

  given Score = scores
  combination.map(score).max

@main def main() =
  val scores: Score =
    fromFile(".cache/13.txt").getLines.toSeq.pipe(parseToScore)

  println(part1(scores))
  println(part2(scores))
