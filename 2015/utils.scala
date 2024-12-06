package utils

final case class Pos(x: Int, y: Int):
  def +(p: Pos) = Pos(x + p.x, y + p.y)

final case class Size(width: Int, height: Int):
  inline def contains(p: Pos) = 0 <= p.x && p.x < width && 0 <= p.y && p.y < height
  inline def apply(p: Pos) = contains(p)
