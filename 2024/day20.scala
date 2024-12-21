package `2024`.day20

import prelude.time
import scala.annotation.tailrec

extension [A](x: A) inline def tap[B](f: A => B): A = { f(x); x }

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

object Track:
  def empty = Track(Pos.zero, Pos.zero, Set.empty)
  def parse(input: String) = entries(input).foldLeft(empty) {
    case (track, (c, p)) =>
      c match
        case 'S' => track.copy(start = p)
        case 'E' => track.copy(end = p)
        case '#' => track.copy(walls = track.walls + p)
        case _   => track
  }

case class Track(start: Pos, end: Pos, walls: Set[Pos]):
  lazy val path: Vector[Pos] =
    val visited = collection.mutable.Set(start)
    inline def canMove(p: Pos) = !walls.contains(p) && !visited.contains(p)

    @tailrec def go(xs: List[Pos]): List[Pos] = xs.head match
      case p if p == end => xs
      case p => go(p.neighbors.filter(canMove).tap(visited ++= _).head :: xs)

    go(List(start)).reverseIterator.toVector

  inline val limit = 3

  def cheatedPaths(maxDistance: Int) = (for
    (p0, i) <- path.zipWithIndex
    (p1, j) <- path.drop(i + limit).zipWithIndex
    dist = p0 taxiDist p1 if dist <= maxDistance
    saved = (j + limit) - dist
  yield saved -> (p0 -> p1)).groupMapReduce(_._1)(_ => 1)(_ + _)

@main def main() =
  import io.Source.fromFile
  val input = fromFile(".cache/2024/20.txt").mkString
  println(input)
  val track = Track.parse(input)

  time:
    println(track.cheatedPaths(2).withFilter(_._1 >= 100).map(_._2).sum)
  time:
    println(track.cheatedPaths(20).withFilter(_._1 >= 100).map(_._2).sum)
