package `2024`.day12

import prelude.*
import scala.annotation.tailrec

extension (a: Pos)(using grid: Grid[Char])
  inline infix def ===(b: Pos) = grid.get(a) == grid.get(b)
  inline infix def !==(b: Pos) = !(a === b)

object Garden:
  def parse(input: String) =
    given grid: Grid[Char] = Grid(input)
    val visited = collection.mutable.Set.empty[Pos]

    def getRegion(p: Pos): Set[Pos] =
      val region = collection.mutable.Set.empty[Pos]

      @tailrec def go(xs: Seq[Pos]): Unit = xs match
        case Nil =>
        case x :: xs if visited.add(x) =>
          region += x; go(x.neighbours.filter(_ === p) ++ xs)
        case x :: xs => go(xs)

      go(Seq(p)); region.toSet

    Garden(grid, grid.points.map(getRegion).filter(_.nonEmpty).toSeq)

case class Garden(grid: Grid[Char], regions: Seq[Set[Pos]]):
  given Grid[Char] = grid

  def perimeter(region: Set[Pos]): Int =
    region.sumBy { p => (p !== p.up).toInt + (p !== p.left).toInt } * 2

  def corners(p: Pos): Int =
    val Seq(nw, w, sw, n, s, ne, e, se) = p.neighbours8.map(_ === p)

    (Seq((n && w && !nw), (n && e && !ne), (s && w && !sw), (s && e && !se))
      ++ Seq(!(n || w), !(n || e), !(s || w), !(s || e))).sumBy(_.toInt)

def part1(input: String): Int =
  val garden = Garden.parse(input)

  garden.regions.sumBy { xs => garden.perimeter(xs) * xs.size }

def part2(input: String): Int =
  val garden = Garden.parse(input)

  garden.regions.sumBy { xs => xs.sumBy(garden.corners) * xs.size }

@main def main() =
  val input = readInput(this).mkString

  println(part1(input))
  println(part2(input))
