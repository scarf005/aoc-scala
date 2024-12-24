package `2024`.day20

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.immutable.Range.Inclusive
import prelude.time

extension (x: Int) inline def ±(y: Int) = x - y to x + y
extension (x: Inclusive)
  inline def &(y: Inclusive) = (x.start max y.start) to (x.end min y.end)

opaque type Pos = Int

object Pos:
  val up = Pos(0, -1)
  val down = Pos(0, 1)
  val left = Pos(-1, 0)
  val right = Pos(1, 0)
  val zero = Pos(0, 0)
  inline def apply(x: Int, y: Int): Pos = y << 16 | x

  extension (p: Pos)
    inline def x = p & 0xffff
    inline def y = p >> 16
    inline def neighbors: List[Pos] =
      List(p + up, p + right, p + down, p + left)
    inline def +(q: Pos): Pos = Pos(p.x + q.x, p.y + q.y)
    inline infix def taxiDist(q: Pos) = (p.x - q.x).abs + (p.y - q.y).abs

case class Rect(x: Inclusive, y: Inclusive):
  inline def &(that: Rect) = Rect(x & that.x, y & that.y)

  def iterator: Iterator[Pos] = for
    y <- y.iterator
    x <- x.iterator
  yield Pos(x, y)

object Track:
  def parse(input: String) =
    val lines = input.trim.split('\n')
    val bounds = Rect(0 to lines.head.size - 1, 0 to lines.size - 1)
    val track = Track(Pos.zero, Pos.zero, Set.empty, bounds)
    bounds.iterator.foldLeft(track) { (track, p) =>
      lines(p.y)(p.x) match
        case 'S' => track.copy(start = p)
        case 'E' => track.copy(end = p)
        case '#' => track.copy(walls = track.walls + p)
        case _   => track
    }

case class Track(start: Pos, end: Pos, walls: Set[Pos], bounds: Rect):
  lazy val path: Vector[Pos] =
    inline def canMove(prev: List[Pos])(p: Pos) =
      !walls.contains(p) && Some(p) != prev.headOption

    @tailrec def go(xs: List[Pos]): List[Pos] = xs match
      case Nil                => Nil
      case p :: _ if p == end => xs
      case p :: ys            => go(p.neighbors.filter(canMove(ys)) ++ xs)

    go(List(start)).reverseIterator.toVector

  lazy val zipped = path.zipWithIndex
  lazy val pathMap = zipped.toMap

  def cheatedPaths(maxDist: Int) =
    def radius(p: Pos) =
      (Rect(p.x ± maxDist, p.y ± maxDist) & bounds).iterator
        .filter(p.taxiDist(_) <= maxDist)

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
