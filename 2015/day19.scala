package `2015`.day19

import prelude.*

type Replacements = Map[String, Set[String]]

extension (s: String)
  def replaceMolecules(replacementsMap: Replacements) =
    (for
      (from, tos) <- replacementsMap
      to <- tos
    yield s.transition(from, to)).flatten.toSet

  def transition(from: String, to: String) =
    s.sliding(from.length)
      .zipWithIndex
      .collect { case (`from`, i) => s.patch(i, to, from.length) }
      .toSet

def part1(molecule: String, reqs: Replacements) =
  molecule.replaceMolecules(reqs).size

def part2(molecule: String, reqs: Replacements) =
  val revReqs = reqs.toVector
    .flatMap { (key, values) => values.map(value => value -> key) }
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap

  val keepBest = 5
  def step(candidates: Vector[String]) = candidates
    .flatMap(_.replaceMolecules(revReqs))
    .sortBy(_.length)
    .take(keepBest)

  LazyList.iterate(Vector(molecule))(step).takeWhile(!_.contains("e")).size

@main def main() =
  val input = fromFile(".cache/2015/19.txt").getLines.toVector

  val molecule = input.last
  val reqs = input
    .dropRight(2)
    .map(_.trim)
    .collect { case s"$from => $to" => from -> to }
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap

  println(part1(molecule, reqs))
  println(part2(molecule, reqs))
