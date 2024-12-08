package `2015`.day17

import prelude.*
import scala.collection.mutable.Map

trait Pretty[A]:
  extension (x: A) inline def pretty: String

inline given [A]: Pretty[IterableOnce[A]] with
  extension (x: IterableOnce[A])
    inline def pretty = x.iterator.mkString("[", ", ", "]")

trait Logger:
  def log(msg: => Any): Unit

given Logger = NoopLogger

object NoopLogger extends Logger:
  def log(msg: => Any): Unit = {}

object PrintLogger extends Logger:
  def log(msg: => Any): Unit = println(msg)

def part1(target: Int, containers: Iterable[Int])(using logger: Logger) =
  val dp = Map[Int, Int](0 -> 1)
  logger.log(s"target: $target, containers: $containers, dp: ${dp.pretty}")

  for currentSize <- containers do
    for nextSize <- (target to currentSize by -1) do
      for x <- dp.get(nextSize - currentSize) do
        dp(nextSize) = dp.getOrElse(nextSize, 0) + x
        logger.log(
          f"ways to fit $nextSize%2dL => using $currentSize%2dL: x${dp(nextSize)} ($x%2d ways to fit leftover ${nextSize - currentSize}%2dL), dp: ${dp.pretty}",
        )
  dp(target)

def part2(target: Int, containers: Seq[Int])(using logger: Logger) =
  def matches(kinds: Int) =
    containers
      .combinationsRepeating(kinds)
      .tapEach(logger.log(_))
      .count(_.sum == target)
      .tap(logger.log(_))

  (1 to containers.size)
    .map(matches)
    .tapEach(logger.log(_))
    .find(_ > 0)
    .getOrElse(throw new Exception("no match found"))

@main def main() =
  val containers = readInput(this).getLines.map(_.toInt).toSeq
  val target = 150

  println(part1(target, containers))
  println(part2(target, containers))
