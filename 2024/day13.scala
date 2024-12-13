package `2024`.day13

import scala.io.Source.fromFile

object D:
  def unapply(s: String) = s.toIntOption.map(BigDecimal(_))

case class Claw(a: BigDecimal, b: BigDecimal, target: BigDecimal)

case class Machine(l: Claw, r: Claw):
  lazy val tokens = solve.map((x, y) => x * 3 + y).sum
  lazy val solve: Option[(BigInt, BigInt)] =
    val bmul = (l.b * r.a) - (r.b * l.a)
    val tmul = (l.target * r.a) - (r.target * l.a)
    val bUnit = tmul / bmul
    val aUnit = (l.target - l.b * bUnit) / l.a
    Option.when(aUnit.isWhole && bUnit.isWhole)(
      aUnit.toBigInt -> bUnit.toBigInt,
    )

given diff: BigDecimal = BigDecimal(10000000000000L)

object Machine:
  def parse(xs: Vector[String]) = xs match
    case Vector(
          s"Button A: X+${D(a)}, Y+${D(d)}",
          s"Button B: X+${D(b)}, Y+${D(e)}",
          s"Prize: X=${D(c)}, Y=${D(f)}",
        ) =>
      Some(Machine(Claw(a, b, c), Claw(d, e, f)))
    case _ => None

@main def main() =
  val machines = fromFile(".cache/2024/13.txt").mkString
    .split("\n+")
    .toVector
    .grouped(3)
    .flatMap(Machine.parse)
    .toVector

  val part1 = machines.map(_.tokens)
  println(part1.sum)

  val part2 = machines
    .map { m =>
      m.copy(
        l = m.l.copy(target = m.l.target + diff),
        r = m.r.copy(target = m.r.target + diff),
      )
    }
    .map(_.tokens)
  println(part2.sum)
