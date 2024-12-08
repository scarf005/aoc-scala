package prelude

extension (n: Long) def digits: Int = math.log10(n.toDouble).toInt + 1

extension (n: Int)
  inline infix def divmod(d: Int) = (n / d, n % d)
  inline infix def moddiv(d: Int) = (n % d, n / d)

final case class Pos(x: Int, y: Int):
  def +(p: Pos) = Pos(x + p.x, y + p.y)

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
