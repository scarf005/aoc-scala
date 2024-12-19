package `2024`.day17

import prelude.*
import scala.collection.immutable.Queue

enum Code:
  case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

enum Reg(val v: Int):
  case A extends Reg(0)
  case B extends Reg(1)
  case C extends Reg(2)

case class Regs(regs: Vector[Int]):
  def apply(r: Reg): Int = regs(r.v)
  def update(r: Reg, v: Int): Regs = Regs(regs.updated(r.v, v))
object Regs:
  def apply(regs: Int*) = new Regs(regs.toVector)

case class Computer(
  regs: Regs = Regs(0, 0, 0),
  inst: Vector[(Code, Int)],
  output: Queue[Int] = Queue.empty,
  ptr: Int = 0,
):
  extension (r: Reg)
    def <--(v: Int) = copy(regs = (regs(r) = v), ptr = ptr + 1)
    def get: Int = regs(r)

  extension (op: Int)
    def combo: Int = op match
      case 4 => Reg.A.get
      case 5 => Reg.B.get
      case 6 => Reg.C.get
      case 7 => ???
      case _ => op
    def div = Reg.A.get / (2 ** op.combo)
    def modulo = op.combo & 0b111

  def unfold: Iterator[Computer] =
    Iterator.unfold(this)(_.next.map(x => (x, x)))
  def run: Computer = unfold.foldLeft(this)((_, current) => current)

  def next: Option[Computer] =
    inst.lift(ptr).map { (code, op) => next(code, op) }

  def next(code: Code, op: Int): Computer = code match
    case Code.Adv => Reg.A <-- op.div
    case Code.Bdv => Reg.B <-- op.div
    case Code.Cdv => Reg.C <-- op.div
    case Code.Bxl => Reg.B <-- (Reg.B.get ^ op)
    case Code.Bst => Reg.B <-- op.modulo
    case Code.Bxc => Reg.B <-- (Reg.B.get ^ Reg.C.get)
    case Code.Jnz => copy(ptr = if Reg.A.get == 0 then ptr + 1 else op)
    case Code.Out => copy(output = output :+ op.modulo, ptr = ptr + 1)

  def pretty = s"""
    ${ptr.toString.padTo(3, ' ')} @ ${regs.regs
      .map(x => s"${x.toBinaryString} ($x)")
      .mkString(", ")}
    $output >>
    $inst
  """.dedent

object Computer:
  def parse(input: String): Computer = input match
    case s"""Register A: ${I(a)}
Register B: ${I(b)}
Register C: ${I(c)}

Program: $xs""" => Computer(Regs(a, b, c), `2024`.day17.parse(xs))

def parse(input: String): Vector[(Code, Int)] = input
  .split(",")
  .map(_.toInt)
  .grouped(2)
  .collect { case Array(c, o) => (Code.fromOrdinal(c), o) }
  .toVector

@main def main() =
  val input = readInput(this).mkString.trim
  val c = Computer.parse(input)
  println(c.pretty)
  val res = c.run
  println(res.pretty)
  res.output.mkString(",") |> println
