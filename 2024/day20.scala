package `2024`.day20

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.immutable.Range.Inclusive
import prelude.time

extension [A](x: A) inline def tap[B](f: A => B): A = { f(x); x }
extension (x: Int) inline def ±(y: Int) = x - y to x + y
extension (x: Inclusive)
  inline def &(y: Inclusive) = (x.start max y.start) to (x.end min y.end)

opaque type Pos = (Int, Int)

object Pos:
  val up: Pos = (0, -1)
  val down: Pos = (0, 1)
  val left: Pos = (-1, 0)
  val right: Pos = (1, 0)
  val zero: Pos = (0, 0)
  def apply(x: Int, y: Int): Pos = (x, y)

  extension (p: Pos)
    inline def x = p._1
    inline def y = p._2
    inline def neighbors: List[Pos] =
      List(p + up, p + right, p + down, p + left)
    inline def +(q: Pos): Pos = (p.x + q.x, p.y + q.y)
    inline infix def taxiDist(q: Pos) = (p.x - q.x).abs + (p.y - q.y).abs

def entries(input: String): Iterator[(Char, Pos)] = for
  (line, y) <- input.linesIterator.zipWithIndex
  (c, x) <- line.iterator.zipWithIndex
yield c -> Pos(x, y)

case class Size(width: Int, height: Int):
  val widthRange = 0 to width - 1
  val heightRange = 0 to height - 1

object Track:
  def parse(input: String) =
    val lines = input.split('\n')
    val size = Size(lines.head.size, lines.size)
    entries(input).foldLeft(Track(Pos.zero, Pos.zero, Set.empty, size)) {
      case (track, (c, p)) =>
        c match
          case 'S' => track.copy(start = p)
          case 'E' => track.copy(end = p)
          case '#' => track.copy(walls = track.walls + p)
          case _   => track
    }

case class Track(start: Pos, end: Pos, walls: Set[Pos], bounds: Size):
  lazy val path: Vector[Pos] =
    val visited = collection.mutable.Set(start)
    inline def canMove(p: Pos) = !walls.contains(p) && !visited.contains(p)

    @tailrec def go(xs: List[Pos]): List[Pos] = xs.head match
      case p if p == end => xs
      case p => go(p.neighbors.filter(canMove).tap(visited ++= _).head :: xs)

    go(List(start)).reverseIterator.toVector

  lazy val zipped = path.zipWithIndex
  lazy val pathMap = zipped.toMap

  def cheatedPaths(maxDistance: Int) =
    def radius(p: Pos) = for
      x <- ((p.x ± maxDistance) & bounds.widthRange).iterator
      y <- ((p.y ± maxDistance) & bounds.heightRange).iterator
      pos = Pos(x, y) if (pos taxiDist p) <= maxDistance
    yield pos

    zipped.par.map { (p, i) =>
      radius(p)
        .flatMap(pathMap.get)
        .map { j => (j - i) - (p taxiDist path(j)) }
        .count(_ >= 100)
    }.sum

@main def main() =
  import io.Source.fromFile
  val input = fromFile(".cache/2024/20.txt").mkString
  val track = Track.parse(input)

  time:
    println(track.cheatedPaths(2))
  time:
    println(track.cheatedPaths(20))
