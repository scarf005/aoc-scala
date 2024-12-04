package `2024`.day04

import prelude.*
import utils.Size

extension [A](xss: Seq[Seq[A]])
  def rightDiagonal(size: Int): Seq[Seq[A]] = for
    y <- 0 to xss.size - size
    x <- size - 1 until xss(0).size
  yield (for i <- 0 until size yield xss(y + i)(x - i))

  def leftDiagonal(size: Int): Seq[Seq[A]] = for
    y <- 0 to xss.size - size
    x <- 0 to xss(0).size - size
  yield (for i <- 0 until size yield xss(y + i)(x + i))

  def rects(size: Size): Seq[Seq[Seq[A]]] =
    for
      y <- 0 to xss.size - size.height
      x <- 0 to xss(0).size - size.width
    yield xss.slice(y, y + size.height).map(_.slice(x, x + size.width))

def anyway(x: String)(y: String): Boolean = x == y || x.reverse == y

def part1(xxs: Seq[Seq[Char]]): Int =
  def countOccurances(xs: Seq[Char]) =
    xs.mkString.sliding(4).count(anyway("XMAS"))

  (xxs ++ xxs.transpose ++ xxs.rightDiagonal(4) ++ xxs.leftDiagonal(4))
    .sumBy(countOccurances)

def part2(xxs: Seq[Seq[Char]]): Int = xxs.rects(Size(3, 3)).count {
  case Seq(
        Seq(a, _, b),
        Seq(_, c, _),
        Seq(d, _, e),
      ) =>
    List(s"$a$c$e", s"$b$c$d").forall(anyway("MAS"))
}

@main def main() =
  val input = readInput(this).getLines.map(_.toVector).toVector

  println(part1(input))
  println(part2(input))
