package utils

import prelude.{Dir, Pos, Size}
import scala.reflect.ClassTag

trait GridOps[A: ClassTag]:
  def s: Size
  def raw: Array[A]

  def apply(pos: Pos): A

  def update(pos: Pos, a: A): Unit

  override def toString(): String =
    val sb = new StringBuilder
    for y <- 0 until s.height do
      for x <- 0 until s.width do
        sb.append(apply(Pos(y, x)) match
          case null => ' '
          case a    => a.toString.head,
        )
      sb.append('\n')
    sb.toString

  def data: Array[Array[A]] = raw.grouped(s.width).toArray
  def get(pos: Pos): Option[A] =
    if pos.y >= 0 && pos.y < s.height && pos.x >= 0 && pos.x < s.width then
      Some(apply(pos))
    else None

  def of(grid: Seq[Seq[A]]): Grid[A] =
    val s = Size(grid(0).length, grid.length)
    val g = new Grid[A](s)
    for
      y <- 0 until s.height
      x <- 0 until s.width
    do g(Pos(y, x)) = grid(y)(x)
    g

class Grid[A: ClassTag](val s: Size) extends GridOps[A]:
  val raw = new Array[A](s.width * s.height)
  def apply(pos: Pos): A = raw(pos.y * s.width + pos.x)
  def update(pos: Pos, a: A): Unit = raw(pos.y * s.width + pos.x) = a

  override def equals(that: Any): Boolean = that match
    case that: Grid[?] => this.s == that.s && this.raw.sameElements(that.raw)
    case _             => false
