package day6
import scala.util.chaining.*
import scala.io.Source.fromFile

import utils.*
import java.awt.image.BufferedImage

import cats.parse.Parser

enum Action:
  case On, Off, Toggle

final case class Selection(begin: Point, end: Point)

final case class Instruction(action: Action, selection: Selection)

type RawGrid[T] = Array[Array[T]]

class Grid[T](
  val underlying: RawGrid[T],
  val updatePixel: (Action, T) => T,
  val getColor: RawGrid[T] => T => java.awt.Color,
  val getSolution: RawGrid[T] => Int,
):
  def width = underlying.head.length
  def height = underlying.length
  def solution = getSolution(underlying)
  def update(instruction: Instruction): Unit =
    val Instruction(action, Selection(Point(x1, y1), Point(x2, y2))) =
      instruction

    for
      x <- x1 to x2
      y <- y1 to y2
    do underlying(y)(x) = updatePixel(action, underlying(y)(x))

  def toBufferedImage: BufferedImage =
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    val color = getColor(underlying)
    for
      x <- 0 until width
      y <- 0 until height
    do
      val p = underlying(y)(x)
      g.setColor(color(p))
      g.fillRect(x, y, 1, 1)
    g.dispose()

    image

import cats.parse.Parser.*
import cats.parse.Rfc5234.{digit, sp}

val number: Parser[Int] =
  digit.rep.string.map(_.toInt)

val coord: Parser[Point] =
  (number <* char(',')) ~ number map Point.apply

val action: Parser[Action] =
  string("turn on").as(Action.On)
    | string("turn off").as(Action.Off)
    | string("toggle").as(Action.Toggle)

val selection: Parser[Selection] =
  (coord <* string("through").surroundedBy(sp.rep0)) ~ coord map Selection.apply

val instruction: Parser[Instruction] =
  action ~ selection.surroundedBy(sp.rep0) map Instruction.apply

def part1(xs: Iterable[Instruction]) =
  Grid[Boolean](
    Array.fill(1000, 1000)(false),
    updatePixel = (action, prev) => {
      action match
        case Action.On     => true
        case Action.Off    => false
        case Action.Toggle => !prev
    },
    getColor = (_) => {
      case true => java.awt.Color.WHITE; case false => java.awt.Color.BLACK
    },
    getSolution = _.flatten.count(identity),
  ).tap { g => xs.foreach(g.update) }

def part2(xs: Iterable[Instruction]) =
  Grid[Int](
    Array.fill(1000, 1000)(0),
    updatePixel = (action, prev) =>
      action match {
        case Action.On     => prev + 1
        case Action.Off    => (prev - 1) max 0
        case Action.Toggle => prev + 2
      },
    getColor = grid => {
      val max = grid.flatten.max
      i => {
        val c = (i * 255.0 / max).toInt
        java.awt.Color(c, c, c / 2)
      }
    },
    getSolution = _.flatten.sum,
  ).tap { g => xs.foreach(g.update) }

@main def main() =
  import com.sksamuel.scrimage.ImmutableImage
  import com.sksamuel.scrimage.webp.WebpWriter

  val input = fromFile(".cache/06.txt")
    .getLines()
    .flatMap(instruction.parseAll)
    .toVector

  Vector(("part1", part1), ("part2", part2)).foreach { (name, f) =>
    val grid = f(input)
    println(s"$name: ${grid.solution}")

    ImmutableImage
      .fromAwt(grid.toBufferedImage)
      .output(
        WebpWriter.MAX_LOSSLESS_COMPRESSION,
        s"img/2015/day6.$name.webp",
      )
  }
