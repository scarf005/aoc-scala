package day11

import prelude.*
import scala.annotation.{nowarn, tailrec}

@nowarn
def inc(sx: List[Char]): List[Char] = sx match
  case Nil          => Nil
  case List('z')    => List('a', 'a')
  case head :+ 'z'  => inc(head) :+ 'a'
  case head :+ tail => head :+ (tail + 1).toChar

val triplets = ('a' to 'z').sliding(3).map(_.mkString).mkString("|").r
val forbidden = "[iol]".r
val parts = raw"(.)\1".r

def secure(s: String) =
  triplets.findFirstIn(s).nonEmpty
    && forbidden.findFirstIn(s).isEmpty
    && parts.findAllIn(s).size > 1

def solution(s: String): String =
  val it = Iterator.iterate(inc(s.toList))(inc)

  it.find { x => secure(x.mkString) }.get.mkString

@main def main() =
  val input = fromFile(".cache/2015/11.txt").mkString.trim

  val part1 = solution(input)
  println(part1)

  val part2 = solution(part1)
  println(part2)
