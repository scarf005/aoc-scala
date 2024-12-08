package `2024`.day06

import prelude.*
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

case class Context(size: Size, pos: Pos)(walls: Set[Pos]):
  def at(pos: Pos) = Option.when(size(pos))(walls(pos))
  def walk =
    val visited = collection.mutable.Set.empty[Pos]

    @tailrec def loop(pos: Pos, dir: Dir): Unit =
      visited.add(pos)
      val next = pos + dir.delta
      at(next) match
        case None       => {}
        case Some(true) => loop(pos, dir.turnRight)
        case Some(_)    => loop(next, dir)

    loop(pos, Dir.Up); visited

  def loops =
    val corners = collection.mutable.Set.empty[(Pos, Dir)]

    @tailrec def loop(pos: Pos, dir: Dir): Boolean =
      val next = pos + dir.delta
      at(next) match
        case None => false
        case Some(true) =>
          (if corners.add(next -> dir) then loop(pos, dir.turnRight) else true)
        case Some(_) => loop(next, dir)

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
  val visited = ctx(walls).walk
  val loops = visited.toArray.par.map { p => ctx(walls + p).loops.toInt }.sum
  visited.size -> loops

@main def main() = readInput(this).mkString |> solve |> println
