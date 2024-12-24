package `2024`.day24

import prelude.*
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

type Wire = Option[Boolean]

enum Op:
  case And, Or, Xor
object Op:
  def parse(s: String): Op = s match
    case "AND" => And
    case "OR"  => Or
    case "XOR" => Xor

case class Gate(a: String, op: Op, b: String)

case class Device(
  wires: collection.mutable.Map[String, Wire],
  gates: Map[String, Gate],
):
  extension (g: Gate)
    def eval =
      val (a, b) = (getWire(g.a), getWire(g.b))
      g.op match
        case Op.And => a & b
        case Op.Or  => a | b
        case Op.Xor => a ^ b

  def getWire(wire: String): Boolean = wires(wire) match
    case Some(x) => x
    case None    => wires.getOrElseUpdate(wire, Some(gates(wire).eval)).get

object Device:
  def parse(input: String) = input.split("\n\n") match
    case Array(a, b) =>
      Device(
        a.linesIterator
          .collect { case s"$k: $v" => k -> Option(v.toInt == 1) }
          .to(collection.mutable.Map)
          .withDefaultValue(None),
        b.linesIterator.collect { case s"$a $op $b -> $c" =>
          c -> Gate(a, Op.parse(op), b)
        }.toMap,
      )

@main def main() =
  val input = fromFile(".cache/2024/24.txt").mkString.trim
  val device = Device.parse(input)

  val zs = device.gates.keys.filter(_.startsWith("z")).toList.sorted
  val asBinary =
    zs.map(device.getWire)
      .zipWithIndex
      .map((b, i) => if b then 1L << i else 0)
      .sum

  println(asBinary)
