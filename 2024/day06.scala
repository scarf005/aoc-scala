package `2024`.day06

import prelude.*
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

extension (d: Dir)
  def rotateRight = d match
    case Dir.Up    => Dir.Right
    case Dir.Down  => Dir.Left
    case Dir.Left  => Dir.Up
    case Dir.Right => Dir.Down

case class Context(size: Size, pos: Pos):
  def walk(walls: Set[Pos]) =
    val visited = collection.mutable.Set.empty[Pos]

    @tailrec def loop(pos: Pos, dir: Dir): Unit =
      visited.add(pos)
      val next = pos + dir.delta
      if !size(next) then {} else if walls(next) then loop(pos, dir.rotateRight)
      else loop(next, dir)

    loop(pos, Dir.Up)
    visited

  def loops(walls: Set[Pos]) =
    val corners = collection.mutable.Set.empty[(Pos, Dir)]

    @tailrec def loop(pos: Pos, dir: Dir): Boolean =
      val next = pos + dir.delta
      if !size(next) then false
      else if walls(next) then
        (if corners.add(next -> dir) then loop(pos, dir.rotateRight) else true
      )
      else loop(next, dir)

    loop(pos, Dir.Up)

def solve(input: String) =
  val grid = input.linesIterator.map(_.toCharArray).toArray
  val size = Size(grid)
  val pos = Pos.apply tupled (input.indexOf('^') moddiv (size.width + 1))
  val walls = (for
    y <- grid.indices; x <- grid(0).indices
    if grid(y)(x) == '#'
  yield Pos(x, y)).toSet

  val ctx = Context(size, pos)
  val visited = ctx.walk(walls)
  val loops = visited.toArray.par.map { p => ctx.loops(walls + p).toInt }.sum
  visited.size -> loops

@main def main() = readInput(this).mkString |> solve |> println
