package utils

final case class Pos(x: Int, y: Int):
  def +(p: Pos) = Pos(x + p.x, y + p.y)

final case class Size(width: Int, height: Int)
