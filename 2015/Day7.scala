package day7

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.HashMap
import scala.io.Source.fromFile
import scala.util.chaining.*
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

object Parser extends RegexParsers {
  def number = raw"\d+".r ^^ (x => UInt16(x.toInt))
  def signal = raw"\d+".r ^^ (x => Expr.Emit(UInt16(x.toInt)))
  def string = raw"[a-z]+".r
  def wire = string ^^ (x => Expr.Var(x))
  def value = signal | wire

  def to = "->" ~ string ^^ { case _ ~ w => w }

  def connection = value ~ to ^^ { case from ~ to => (to -> from) }

  def and = value ~ "AND" ~ value ~ to
    ^^ { case left ~ _ ~ right ~ to => (to -> Expr.And(left, right)) }

  def or = value ~ "OR" ~ value ~ to
    ^^ { case left ~ _ ~ right ~ to => (to -> Expr.Or(left, right)) }

  def lshift = value ~ "LSHIFT" ~ number ~ to
    ^^ { case value ~ _ ~ by ~ to => (to -> Expr.LShift(value, by)) }

  def rshift = value ~ "RSHIFT" ~ number ~ to
    ^^ { case value ~ _ ~ by ~ to => (to -> Expr.RShift(value, by)) }

  def not = "NOT" ~ value ~ to
    ^^ { case _ ~ value ~ to => to -> Expr.Not(value) }

  def ops = connection | and | or | lshift | rshift | not

  def parseAll(input: Iterable[String]) =
    input
      .map(parse(ops, _))
      .collect { case Success(result, _) => result }
      .toMap
}

def evaluate(wires: Map[Wire, Expr], expr: Expr) =
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
    fromFile(".cache/07.txt").getLines.toVector.sorted.pipe(Parser.parseAll)

  val a = solve(input, "a")
  println(s"a = $a")

  val newInput = input.updated("b", Expr.Emit(a))
  val newA = evaluate(newInput, Expr.Var("a"))
  println(s"a (part 2) = $newA")
