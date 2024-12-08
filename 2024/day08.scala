package `2024`.day08

import prelude.*

extension (a: Pos)
  infix def deltas(b: Pos): Iterator[Pos] =
    val delta = (b - a)
    Iterator.iterate(a + delta)(_ + delta)

case class Context(size: Size, antennas: Map[Char, Vector[Pos]]):
  def solve(fn: ((Pos, Pos)) => IterableOnce[Pos]) =
    antennas.values.flatMap(_.combinationsN(2).flatMap(fn)).toSet

  lazy val part1 =
    solve((a, b) => Vector(a + (a - b), b + (b - a))).filter(size(_)).size
  lazy val part2 = solve((a, b) =>
    (a deltas b).takeWhile(size(_)) ++ (b deltas a).takeWhile(size(_)),
  ).size

object Context:
  def apply(input: String): Context =
    apply(input.linesIterator.map(_.toCharArray).toArray)

  def apply(grid: Array[Array[Char]]): Context =
    val size = Size(grid)
    val antennas = (for
      y <- 0 until size.height
      x <- 0 until size.width
      c = grid(y)(x)
      if c != '.' && c != '#'
    yield c -> Pos(x, y)).toVector.groupMap(_._1)(_._2)
    Context(size, antennas)

@main def main() =
  val input = readInput(this).mkString
  val ctx = Context(input)

  println(ctx.part1)
  println(ctx.part2)
