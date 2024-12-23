package `2015`.day03

import scala.collection.mutable.HashSet

import prelude.*

def parse(c: Char) = c match
  case '^' => Pos(x = 0, y = 1)
  case 'v' => Pos(x = 0, y = -1)
  case '<' => Pos(x = -1, y = 0)
  case '>' => Pos(x = 1, y = 0)

def part1(input: String) =
  val visited = HashSet(Pos(0, 0))
  var current = Pos(0, 0)
  input.map(parse).foreach { p =>
    current += p
    visited.add(current)
  }
  visited.size

def part2(input: String) =
  val visited = HashSet(Pos(0, 0))
  var santa = Pos(0, 0)
  var robo = Pos(0, 0)
  input.map(parse).zipWithIndex.foreach { (p, i) =>
    if i % 2 == 0 then santa += p else robo += p
    visited.add(santa)
    visited.add(robo)
  }
  visited.size

@main def main() =
  val input = readInput(this).mkString

  println(part1(input))
  println(part2(input))
