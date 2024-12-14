package `2024`.day14

import prelude.*
import java.awt.image.BufferedImage
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.webp.WebpWriter

given size: Size = Size(101, 103)

case class Robot(p: Pos, v: Pos):
  def at(t: Int)(using size: Size): Pos =
    val x = {
      val x = (p.x + t * v.x) % size.width
      if x < 0 then size.width + x else x
    }
    val y = {
      val y = (p.y + t * v.y) % size.height
      if y < 0 then size.height + y else y
    }
    Pos(x, y)

object I:
  def unapply(s: String): Option[Int] = s.toIntOption

def parse(input: String) =
  input
    .split("\n")
    .toSeq
    .collect { case s"p=${I(x)},${I(y)} v=${I(vx)},${I(vy)}" =>
      Robot(Pos(x, y), Pos(vx, vy))
    }

enum Quadrant:
  case I, II, III, IV

def quadrant(p: Pos)(using size: Size): Option[Quadrant] =
  ((p.x - size.width / 2).sign, (p.y - size.height / 2).sign) match
    case (1, 1)   => Some(Quadrant.I)
    case (-1, 1)  => Some(Quadrant.II)
    case (-1, -1) => Some(Quadrant.III)
    case (1, -1)  => Some(Quadrant.IV)
    case _        => None

def asImage(xs: Set[Pos])(using size: Size) =
  val image = BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
  val g = image.createGraphics()
  for
    x <- 0 until size.width
    y <- 0 until size.height
  do
    g.setColor(
      if xs(Pos(x, y)) then java.awt.Color.BLACK else java.awt.Color.WHITE,
    )
    g.fillRect(x, y, 1, 1)
  g.dispose()

  image

def part1(input: String) =
  val robots = parse(input)
  robots.map(_.at(100)).flatMap(quadrant).frequencies.values.reduce(_ * _)

def part2(input: String) =
  val robots = parse(input)

  for t <- 6470 until 6480 do
    val pos = robots.map(_.at(t)).toSet

    ImmutableImage
      .fromAwt(asImage(pos))
      .output(
        WebpWriter.MAX_LOSSLESS_COMPRESSION,
        f"img/2024/14/$t%05d.webp",
      )

@main def main =
  val input = fromFile(".cache/2024/14.txt").mkString

  println(part1(input))

  os.makeDir.all(os.pwd / "img" / "2024" / "14")
  println(part2(input))
