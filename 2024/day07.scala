package `2024`.day07
import prelude.*
import scala.util.Try
import scala.annotation.tailrec

val example = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""

extension [C <: Iterable, A](xs: C[A])
  def orEmpty[B](f: C[A] => B)(els: => B) = if (xs.isEmpty) els else f(xs)

def getCombo(xs: List[Long], next: Long => List[Long]) =
  @tailrec def go(xs: List[Long], acc: List[Long] = Nil): List[Long] =
    xs match
      case Nil      => acc
      case x :: Nil => acc.flatMap(next)
      case x :: ys  => go(ys, acc.orEmpty(_.flatMap(next))(List(x)))
  go(xs)

@tailrec def combo(xs: List[Long], acc: List[Long] = Nil): List[Long] =
  xs match
    case Nil      => acc
    case x :: Nil => acc.flatMap(a => List(a + x, a * x))
    case x :: ys =>
      combo(ys, acc.orEmpty(_.flatMap(a => List(a + x, a * x)))(List(x)))

@tailrec def combo2(xs: List[Long], acc: List[Long] = Nil): List[Long] =
  xs match
    case Nil => acc // When no elements remain, return the accumulated results
    case x :: Nil =>
      acc
        .map(a => List(a + x, a * x, combineDigit(a, x)))
        .flatten // Combine last element with accumulated results
    case x :: ys =>
      combo2(
        ys,
        if (acc.isEmpty) List(x)
        else acc.flatMap(a => List(a + x, a * x, combineDigit(a, x))),
      )

    // combineDigit(12, 345) = 12345
def combineDigit(a: Long, b: Long) = s"$a$b".toLong

@main def main() =
  val input = readInput(this).getLines
    .collect { case s"$target: $xs" =>
      (target.toLong, xs.split(" ").map(_.toLong).toList)
    }
    .toVector
    .filter((target, xs) => getCombo(xs).exists(_ == target))
  println(input.map(_._1).sum)
