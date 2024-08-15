package utils

final case class Point(x: Int, y: Int):
  def +(p: Point) = Point(x + p.x, y + p.y)

final case class Size(width: Int, height: Int)

implicit def either2Iterable[A](e: Either[?, A]): Iterable[A] =
  e.toOption.toIterable
