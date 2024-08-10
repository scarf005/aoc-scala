package util

final case class Point(x: Int, y: Int):
  def +(p: Point) = Point(x + p.x, y + p.y)
