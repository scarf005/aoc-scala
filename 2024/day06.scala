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

def walk(size: Size, pos: Pos)(walls: Set[Pos]) =
  val visited = collection.mutable.Set.empty[(Pos, Dir)]

  @tailrec def loop(pos: Pos, dir: Dir): Boolean =
    val next = pos + dir.delta
    if !size(next) then false
    else if walls(next) then loop(pos, dir.rotateRight)
    else (if visited.add(next -> dir) then loop(next, dir)
          else true)

  loop(pos, Dir.Up) |> (isLoop => visited -> isLoop)

def solve(input: String) =
  val grid = input.linesIterator.map(_.toCharArray).toArray
  val size = Size(grid)
  val pos = Pos.apply tupled (input.indexOf('^') moddiv (size.width + 1))
  val walls = (for
    y <- grid.indices; x <- grid(0).indices
    if grid(y)(x) == '#'
  yield Pos(x, y)).toSet

  val ctx = walk(size, pos)
  val visited = ctx(walls)._1.map(_._1).toSet
  val loops = visited.toArray.par.map { p => ctx(walls + p)._2.toInt }.sum
  visited.size -> loops

@main def main() = readInput(this).mkString |> solve |> println
