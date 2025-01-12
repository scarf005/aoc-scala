package `2024`.day23

import prelude.*
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

type Connection = Map[String, Set[String]]

def parse(input: String): Connection = input
  .split('\n')
  .toSet
  .flatMap { case s"$a-$b" => Set(a -> b, b -> a) }
  .groupMap(_._1)(_._2)

def part1(input: String) =
  val connection = parse(input)

  extension (a: String)
    inline infix def <->(b: String) =
      connection(a).contains(b) && connection(b).contains(a)

  def isValidTriangle(vertices: Set[String]): Boolean = vertices.toList match
    case List(a, b, c) => a <-> b && b <-> c && c <-> a
    case _             => false

  connection.par
    .flatMap { (vertex, neighbors) =>
      neighbors
        .subsets(2)
        .map(_ + vertex)
        .withFilter(_.exists(_.startsWith("t")))
        .filter(isValidTriangle)
    }
    .toSet
    .size

def part2(input: String) =
  val connection = parse(input)
  findMaximumCliqueBronKerbosch(connection).toList.sorted.mkString(",")

def findMaximumCliqueBronKerbosch(connections: Connection): Set[String] =
  def bronKerbosch(
    potential: Set[String],
    excluded: Set[String] = Set.empty,
    result: Set[String] = Set.empty,
  ): Set[String] =
    if (potential.isEmpty && excluded.isEmpty) then result
    else
      // Choose pivot to minimize branching
      val pivot = (potential ++ excluded)
        .maxBy(vertex => potential.count(connections(vertex).contains))

      val remaining = potential -- connections(pivot)

      remaining.foldLeft(Set.empty[String]) { (currentMax, vertex) =>
        val neighbors = connections(vertex)
        val newClique = bronKerbosch(
          result = result + vertex,
          potential = potential & neighbors,
          excluded = excluded & neighbors,
        )
        if (newClique.size > currentMax.size) newClique else currentMax
      }

  bronKerbosch(potential = connections.keySet)

@main def main() =
  val input = fromFile(".cache/2024/23.txt").mkString
  println(part1(input))
  println(part2(input))
