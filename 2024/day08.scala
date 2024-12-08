package `2024`.day08

import prelude.*

extension (a: Pos)
  infix def deltas(b: Pos): Iterator[Pos] =
    val delta = (b - a)
    Iterator.iterate(a + delta)(_ + delta)

case class Context(size: Size, antennaes: Map[Char, Vector[Pos]]):
  def solve(fn: ((Pos, Pos)) => IterableOnce[Pos]) =
    antennaes.values.flatMap(_.combinationsN(2).flatMap(fn)).toSet

  lazy val part1 =
    solve((a, b) => Vector(a + (a - b), b + (b - a))).filter(size.contains).size
  lazy val part2 = solve((a, b) =>
    (a deltas b).takeWhile(size.contains)
      ++ (b deltas a).takeWhile(size.contains),
  ).size

object Context:
  def apply(input: String): Context =
    apply(input.linesIterator.map(_.toCharArray).toArray)

  def apply(grid: Array[Array[Char]]): Context =
    val size = Size(grid)
    val antennaes = (for
      y <- 0 until size.height
      x <- 0 until size.width
      c = grid(y)(x)
      if c != '.' && c != '#'
    yield c -> Pos(x, y)).toVector.groupMap(_._1)(_._2)
    Context(size, antennaes)

@main def main() =
  val input = readInput(this).mkString
  val ctx = Context(input)

  println(ctx.part1)
  println(ctx.part2)
