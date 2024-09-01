package day8

import scala.io.Source.fromFile
import scala.util.chaining.*

extension (s: String)
  def memoryLen = decode(s.toList).size - 2
  def encodedLen = encode(s.toList).size + 2

def decode(sx: List[Char]): List[Char] = sx match
  case '\\' :: '\\' :: xs          => '\\' +: decode(xs)
  case '\\' :: '"' :: xs           => '"' +: decode(xs)
  case '\\' :: 'x' :: _ :: _ :: xs => 'x' +: decode(xs)
  case x :: xs                     => x +: decode(xs)
  case Nil                         => Nil

def encode(sx: List[Char]): List[Char] = sx match
  case '\\' :: xs => '\\' +: '\\' +: encode(xs)
  case '"' :: xs  => '\\' +: '"' +: encode(xs)
  case x :: xs    => x +: encode(xs)
  case Nil        => Nil

def part1(s: String) = s.size - s.memoryLen
def part2(s: String) = s.encodedLen - s.size

@main def main() =
  val input =
    fromFile(".cache/08.txt").getLines.toVector.map(_.trim)

  input.map(part1).sum.pipe(println)
  input.map(part2).sum.pipe(println)
