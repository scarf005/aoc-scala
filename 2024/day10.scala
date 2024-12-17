package `2024`.day10

import prelude.*

case class Context(grid: Grid[Int]):
  def next(x: Pos): Seq[Pos] =
    if grid(x) == 9 then Seq(x)
    else
      x.neighbours
        .withFilter(y => grid.size(y) && grid(y) == grid(x) + 1)
        .flatMap(next)

  lazy val trails = grid.filter(_ == 0).map(next)
  lazy val part1 = trails.sumBy(_.toSet.size)
  lazy val part2 = trails.sumBy(_.size)

  override def toString = grid.repr
    .map(xs => xs.map(x => (if x >= 0 then x else '.')).mkString)
    .mkString("\n")

@main def main() =
  val input = readInput(this).mkString
  val ctx = Context(Grid(input).map(_.asDigit))

  println(ctx)
  println(ctx.grid.size)
  println(ctx.part1)
  println(ctx.part2)
