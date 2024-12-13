package `2024`.day13

import scala.io.Source.fromFile

extension (a: Long)
  infix def safeDiv(b: Long): Option[Long] =
    Option.when(b != 0 && a % b == 0)(a / b)

case class Claw(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long):
  // A⋅ax + B⋅bx = x, A⋅ay + B⋅by = y
  // A = (x - B⋅bx) / ax, A  = (y - B⋅by) / ay
  // (x - B⋅bx) / ax = (y - B⋅by) / ay
  // (x - B⋅bx)⋅ay = (y - B⋅by)⋅ax
  // x⋅ay - B⋅bx⋅ay = y⋅ax - B⋅by⋅ax
  // (x⋅ay - y⋅ax) = B⋅(bx⋅ay - by⋅ax)
  //
  // B = (x⋅ay - y⋅ax) / (bx⋅ay - by⋅ax)
  // A = (x - B⋅bx) / ax
  def solve: Option[Long] = for
    b <- (x * ay - y * ax) safeDiv (bx * ay - by * ax)
    a <- (x - b * bx) safeDiv ax
  yield a * 3 + b

object L:
  def unapply(s: String): Option[Long] = s.toLongOption

object Claw:
  def parse(xs: Seq[String]): Option[Claw] = xs match
    case Seq(
          s"Button A: X+${L(ax)}, Y+${L(ay)}",
          s"Button B: X+${L(bx)}, Y+${L(by)}",
          s"Prize: X=${L(x)}, Y=${L(y)}",
        ) =>
      Some(Claw(ax, ay, bx, by, x, y))
    case _ => None

def parse(input: String): Seq[Claw] =
  input.split("\n+").toSeq.grouped(3).flatMap(Claw.parse).toSeq

def part1(input: String): Long =
  parse(input).flatMap(_.solve).sum

def part2(input: String): Long =
  val diff = 10_000_000_000_000L
  parse(input)
    .map(c => c.copy(x = c.x + diff, y = c.y + diff))
    .flatMap(_.solve)
    .sum

@main def main() =
  val input = fromFile(".cache/2024/13.txt").mkString
  println(part1(input))
  println(part2(input))
