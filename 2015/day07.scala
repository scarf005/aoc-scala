package `2015`.day07

import scala.collection.mutable.HashMap
import prelude.*
import prelude.parser.*
import utils.uint.*

type Wire = String
type Signal = UInt16

enum Expr:
  case Var(name: Wire)
  case Emit(signal: Signal)
  case And(left: Expr, right: Expr)
  case Or(left: Expr, right: Expr)
  case LShift(wire: Expr, by: UInt16)
  case RShift(wire: Expr, by: UInt16)
  case Not(from: Expr)

object Parser {
  import cats.parse.Parser
  import cats.parse.Parser.*
  import cats.parse.Rfc5234.*

  val u16: Parser[UInt16] = number.map(UInt16.apply)
  val signal: Parser[Expr] = u16.map(Expr.Emit.apply)
  val literal: Parser[String] = charIn('a' to 'z').rep.string
  val wire: Parser[Expr] = literal.map(Expr.Var.apply)
  val value: Parser[Expr] = signal | wire

  val to: Parser[String] = keyword("->") *> literal
  val connection = value ~ to
  val and = ((value <* keyword("AND")) ~ value).map(Expr.And.apply) ~ to
  val or = ((value <* keyword("OR")) ~ value).map(Expr.Or.apply) ~ to
  val lshift =
    ((value <* keyword("LSHIFT")) ~ u16).map(Expr.LShift.apply) ~ to
  val rshift =
    ((value <* keyword("RSHIFT")) ~ u16).map(Expr.RShift.apply) ~ to
  val not = (keyword("NOT") *> value).map(Expr.Not.apply) ~ to
  val ops =
    oneOf(List(connection, and, or, lshift, rshift, not).map(_.backtrack))

  def parseToMap(input: Iterable[String]) =
    input.flatMap(ops.parseAll).map { (expr, to) => to -> expr }.toMap
}

def evaluate(wires: Map[Wire, Expr], expr: Expr): Signal =
  val mutWires = scala.collection.mutable.Map() ++ wires

  def eval(expr: Expr): Signal =
    import Expr.*
    expr match
      case Emit(value) => value
      case Var(name) =>
        val value = eval(mutWires(name))
        mutWires(name) = Emit(value)
        value
      case And(left, right) => eval(left) & eval(right)
      case Or(left, right)  => eval(left) | eval(right)
      case LShift(wire, by) => eval(wire) << by
      case RShift(wire, by) => eval(wire) >> by
      case Not(from)        => ~eval(from)

  eval(expr)

def solve(wires: Map[Wire, Expr], name: Wire) =
  evaluate(wires, Expr.Var(name))

@main def main() =
  val input =
    readInput(this).getLines.toVector.pipe(Parser.parseToMap)

  val a = solve(input, "a")
  println(s"a = $a")

  val newInput = input.updated("b", Expr.Emit(a))
  val newA = evaluate(newInput, Expr.Var("a"))
  println(s"a (part 2) = $newA")

  assert(a == UInt16(16076))
  assert(newA == UInt16(2797))
