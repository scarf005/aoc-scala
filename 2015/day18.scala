package `2015`.day18

import prelude.*
import utils.*

enum Cell:
  case On, Off
  override def toString = if this == Cell.On then "#" else "."

extension (sc: StringContext)
  def gol(args: Any*): GameOfLifeGrid =
    sc.s(args*).trim.stripMargin |> GameOfLifeGrid.of

class GameOfLifeGrid(override val s: Size) extends utils.Grid[Cell](s):
  def apply(buf: utils.Grid[Cell]) =
    for
      y <- 0 until s.height
      x <- 0 until s.width
    do this(Pos(y, x)) = buf(Pos(y, x))

  def neighbors(pos: Pos): Int =
    val coords = for
      dy <- -1 to 1
      dx <- -1 to 1
      if dy != 0 || dx != 0
    yield pos + Pos(dy, dx)
    coords.map(this.get).flatten.count(_ == Cell.On)

  def next(pos: Pos): Cell =
    val n = neighbors(pos)
    apply(pos) match
      case Cell.On if n == 2 || n == 3 => Cell.On
      case Cell.Off if n == 3          => Cell.On
      case _                           => Cell.Off

object GameOfLifeGrid:
  def of(other: String): GameOfLifeGrid =
    other.split("\n").toVector |> of

  def of(other: Seq[String]): GameOfLifeGrid =
    val s = Size(other.size, other.head.size)
    val g = new GameOfLifeGrid(s)
    for
      y <- 0 until s.height
      x <- 0 until s.width
    do g(Pos(y, x)) = if other(y)(x) == '#' then Cell.On else Cell.Off
    g

extension (g: GameOfLifeGrid)
  def part1Next: GameOfLifeGrid =
    val nextGrid = new GameOfLifeGrid(g.s)
    for
      y <- 0 until g.s.height
      x <- 0 until g.s.width
    do nextGrid(Pos(y, x)) = g.next(Pos(y, x))
    nextGrid

  def stuckCorners(): GameOfLifeGrid =
    Set(
      Pos(0, 0),
      Pos(0, g.s.width - 1),
      Pos(g.s.height - 1, 0),
      Pos(g.s.height - 1, g.s.width - 1),
    ).foreach { g.update(_, Cell.On) }
    g

  def part2Next: GameOfLifeGrid =
    val nextGrid = new GameOfLifeGrid(g.s)
    g.stuckCorners()
    for
      y <- 0 until g.s.height
      x <- 0 until g.s.width
    do nextGrid(Pos(y, x)) = g.next(Pos(y, x))
    nextGrid.stuckCorners()
    nextGrid

def solution(grid: GameOfLifeGrid, f: GameOfLifeGrid => GameOfLifeGrid) =
  Iterator.iterate(grid)(f).drop(100).next.raw.count(_ == Cell.On)

def part1(grid: GameOfLifeGrid) =
  solution(grid, _.part1Next)

def part2(grid: GameOfLifeGrid) =
  solution(grid, _.part2Next)

@main def main() =
  val raw = readInput(this).mkString

  println(part1(GameOfLifeGrid.of(raw)))
  println(part2(GameOfLifeGrid.of(raw)))
