package `2015`.day23

import prelude.*
import collection.mutable.Map

enum Instruction:
  case Half(reg: String)
  case Triple(reg: String)
  case Increment(reg: String)
  case Jump(offset: Int)
  case JumpIfEven(reg: String, offset: Int)
  case JumpIfOne(reg: String, offset: Int)

object Instruction:
  def parse(s: String): Instruction = s match
    case s"hlf $r"          => Half(r)
    case s"tpl $r"          => Triple(r)
    case s"inc $r"          => Increment(r)
    case s"jmp $offset"     => Jump(offset.toInt)
    case s"jie $r, $offset" => JumpIfEven(r, offset.toInt)
    case s"jio $r, $offset" => JumpIfOne(r, offset.toInt)

class Computer(
  regs: Map[String, Int],
  instrs: Vector[Instruction],
  var cursor: Int = 0,
):
  def done = _done
  private var _done = false

  def run(): Computer =
    while !done do next()
    this

  def next() =
    import Instruction.*
    instrs.lift(cursor) match
      case Some(Half(r)) =>
        regs(r) /= 2
        cursor += 1
      case Some(Triple(r)) =>
        regs(r) *= 3
        cursor += 1
      case Some(Increment(r)) =>
        regs(r) += 1
        cursor += 1
      case Some(Jump(offset)) => cursor += offset
      case Some(JumpIfEven(r, offset)) =>
        cursor += (if regs(r) % 2 == 0 then offset else 1)
      case Some(JumpIfOne(r, offset)) =>
        cursor += (if regs(r) == 1 then offset else 1)
      case None => _done = true

  override def toString(): String = s"Computer(regs=$regs)"

@main def main() =
  val instructions =
    readInput(this).getLines.map(Instruction.parse).toVector

  println(Computer(Map("a" -> 0, "b" -> 0), instructions).run())
  println(Computer(Map("a" -> 1, "b" -> 0), instructions).run())
