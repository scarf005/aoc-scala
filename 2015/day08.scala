package `2015`.day08

import prelude.*
import scala.annotation.tailrec

extension (s: String)
  def memoryLen = decode(s.toList).size - 2
  def encodedLen = encode(s.toList).size + 2

@tailrec
def decode(ax: List[Char], acc: List[Char] = List()): List[Char] = ax match
  case '\\' :: '\\' :: xs          => decode(xs, acc :+ '\\')
  case '\\' :: '"' :: xs           => decode(xs, acc :+ '"')
  case '\\' :: 'x' :: _ :: _ :: xs => decode(xs, acc :+ 'x')
  case x :: xs                     => decode(xs, acc :+ x)
  case Nil                         => acc

@tailrec
def encode(ax: List[Char], acc: List[Char] = List()): List[Char] = ax match
  case '\\' :: xs => encode(xs, acc :+ '\\' :+ '\\')
  case '"' :: xs  => encode(xs, acc :+ '\\' :+ '"')
  case x :: xs    => encode(xs, acc :+ x)
  case Nil        => acc

def part1(s: String) = s.size - s.memoryLen
def part2(s: String) = s.encodedLen - s.size

@main def main() =
  val input =
    readInput(this).getLines.toVector.map(_.trim)

  input.map(part1).sum.pipe(println)
  input.map(part2).sum.pipe(println)
