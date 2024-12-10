package prelude

case class Grid[A](repr: Vector[Vector[A]]):
  val size = Size(repr)
  def where(p: A => Boolean): Vector[Pos] =
    for (row, y) <- repr.zipWithIndex; (cell, x) <- row.zipWithIndex if p(cell)
    yield Pos(x, y)

  def apply(p: Pos) = repr(p.y)(p.x)
  def map[B](f: A => B): Grid[B] = Grid(repr.map(_.map(f)))
  def contains(p: Pos) = size.contains(p)
  override def toString: String = repr.map(_.mkString).mkString("\n")

object Grid:
  def apply(s: String) = new Grid(s.linesIterator.map(_.toVector).toVector)
