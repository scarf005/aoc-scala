package prelude

import prelude.Pos.northEast

extension (n: Long) def digits: Int = math.log10(n.toDouble).toInt + 1

extension (n: Int)
  inline infix def divmod(d: Int) = (n / d, n % d)
  inline infix def moddiv(d: Int) = (n % d, n / d)
  inline infix def isOdd = n % 2 == 1
  inline infix def isEven = n % 2 == 0
  inline def **(exp: Int) = math.pow(n, exp).toInt

extension (n: Long)
  inline infix def divmod(d: Long) = (n / d, n % d)
  inline infix def moddiv(d: Long) = (n % d, n / d)
  inline infix def isOdd = n % 2 == 1
  inline infix def isEven = n % 2 == 0
  inline def **(exp: Long) = math.pow(n.toDouble, exp.toDouble).toLong

final case class Pos(x: Int, y: Int):
  inline def +(p: Pos) = Pos(x + p.x, y + p.y)
  inline def -(p: Pos) = Pos(x - p.x, y - p.y)
  inline def *(n: Int) = Pos(x * n, y * n)
  inline infix def manhattan(p: Pos): Int = (x - p.x).abs + (y - p.y).abs
  inline def manhattan: Int = manhattan(Pos.zero)
  def neighbours = Seq(up, down, left, right)
  def diagonals = Seq(northEast, northWest, southEast, southWest)
  def neighbours8 = Seq(nw, w, sw, n, s, ne, e, se)

  def nw = northWest
  def w = left
  def sw = southWest
  def n = up
  def s = down
  def ne = northEast
  def e = right
  def se = southEast

  def up = this + Pos.up
  def down = this + Pos.down
  def left = this + Pos.left
  def right = this + Pos.right
  def northEast = this + Pos.northEast
  def northWest = this + Pos.northWest
  def southEast = this + Pos.southEast
  def southWest = this + Pos.southWest
  def `3x3`: Seq[Seq[Pos]] = Seq(
    Seq(northWest, up, northEast),
    Seq(left, this, right),
    Seq(southWest, down, southEast),
  )

object Pos:
  val zero = Pos(0, 0)
  val up = Pos(0, -1)
  val down = Pos(0, 1)
  val left = Pos(-1, 0)
  val right = Pos(1, 0)
  val northEast = Pos(1, -1)
  val northWest = Pos(-1, -1)
  val southEast = Pos(1, 1)
  val southWest = Pos(-1, 1)

final case class Size(width: Int, height: Int):
  inline def contains(p: Pos) =
    0 <= p.x && p.x < width && 0 <= p.y && p.y < height
  inline def apply(p: Pos) = contains(p)

object Size:
  def apply[A](xs: Seq[Seq[A]]): Size = Size(xs(0).size, xs.size)
  def apply[A](xs: Array[Array[A]]): Size = Size(xs(0).size, xs.size)

enum Dir:
  case Up, Down, Left, Right

  def delta = this match
    case Dir.Up    => Pos.up
    case Dir.Down  => Pos.down
    case Dir.Left  => Pos.left
    case Dir.Right => Pos.right

  def turnRight = this match
    case Dir.Up    => Dir.Right
    case Dir.Down  => Dir.Left
    case Dir.Left  => Dir.Up
    case Dir.Right => Dir.Down

  def turnLeft = this match
    case Dir.Up    => Dir.Left
    case Dir.Down  => Dir.Right
    case Dir.Left  => Dir.Down
    case Dir.Right => Dir.Up

object I:
  def unapply(s: String): Option[Int] = s.toIntOption
