package prelude

extension (n: Long) def digits: Int = math.log10(n.toDouble).toInt + 1

extension (n: Int)
  inline infix def divmod(d: Int) = (n / d, n % d)
  inline infix def moddiv(d: Int) = (n % d, n / d)

final case class Pos(x: Int, y: Int):
  inline def +(p: Pos) = Pos(x + p.x, y + p.y)
  inline def -(p: Pos) = Pos(x - p.x, y - p.y)
  inline def *(n: Int) = Pos(x * n, y * n)
  inline infix def manhattan(p: Pos): Int = (x - p.x).abs + (y - p.y).abs
  inline def manhattan: Int = manhattan(Pos.zero)

object Pos:
  val zero = Pos(0, 0)

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
    case Up    => Pos(0, -1)
    case Down  => Pos(0, 1)
    case Left  => Pos(-1, 0)
    case Right => Pos(1, 0)

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
