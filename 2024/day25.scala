package `2024`.day25

import prelude.*

enum Scheme(xs: Vector[Int]):
  case Lock(xs: Vector[Int]) extends Scheme(xs)
  case Key(xs: Vector[Int]) extends Scheme(xs)

export Scheme.*

extension (l: Lock)
  inline infix def isMatching(k: Key)(using height: Int): Boolean =
    (l.xs zip k.xs).forall((x, y) => x + y <= height)

def parse(input: String): Scheme =
  val lines = input.split('\n').toVector.transpose
  val xs = lines.map(_.count(_ == '#'))
  if lines.head.head == '#' then Lock(xs) else Key(xs)

@main def main() =
  val input = fromFile(".cache/2024/25.txt").mkString.trim
  val xs = input.split("\n\n").toVector

  given height: Int = xs.head.linesIterator.size

  val (locks, keys) = xs.map(parse).partitionMap {
    case lock: Lock => Left(lock)
    case key: Key   => Right(key)
  }
  val combos = locks.flatMap(l => keys.filter(l.isMatching))
  println(combos.size)
