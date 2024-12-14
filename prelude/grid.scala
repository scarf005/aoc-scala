package prelude

case class Grid[A](repr: Vector[Vector[A]]):
  val size = Size(repr)
  def where(p: A => Boolean): Vector[Pos] =
    for (row, y) <- repr.zipWithIndex; (cell, x) <- row.zipWithIndex if p(cell)
    yield Pos(x, y)

  def apply(p: Pos) = repr(p.y)(p.x)

  def get(p: Pos): Option[A] = repr.lift(p.y).flatMap(_.lift(p.x))
  def getOrElse(p: Pos, default: A): A = get(p).getOrElse(default)

  def map[B](f: A => B): Grid[B] = Grid(repr.map(_.map(f)))
  def contains(p: Pos) = size.contains(p)
  override def toString: String = repr.map(_.mkString).mkString("\n")
  def points: Iterator[Pos] = for
    y <- (0 until size.height).iterator
    x <- (0 until size.width).iterator
  yield Pos(x, y)
  def entries: Iterator[(Pos, A)] = points.map(p => p -> apply(p))

object Grid:
  def apply(s: String) = new Grid(s.linesIterator.map(_.toVector).toVector)
