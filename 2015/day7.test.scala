import munit.FunSuite
import y2015.day7.*
import utils.uint.*

class Day7Tests extends FunSuite:
  import Parser.*

  test("atoms"):
    assertEquals(number.parse("123"), Right("", UInt16(123)))
    assertEquals(signal.parse("123"), Right("", Expr.Emit(UInt16(123))))
    assertEquals(wire.parse("abc"), Right("", Expr.Var("abc")))

  test("connection"):
    assertEquals(
      connection.parse("abc -> def"),
      Right("", Expr.Var("abc") -> "def"),
    )
    assertEquals(
      connection.parse("123 -> abc"),
      Right("", Expr.Emit(UInt16(123)) -> "abc"),
    )

  test("ops"):
    assertEquals(
      ops.parse("123 -> abc"),
      Right("", Expr.Emit(UInt16(123)) -> "abc"),
    )
    assertEquals(
      ops.parse("123 AND abc -> def"),
      Right("", Expr.And(Expr.Emit(UInt16(123)), Expr.Var("abc")) -> "def"),
    )
    // x AND y -> d
    assertEquals(
      ops.parse("x AND y -> d"),
      Right("", Expr.And(Expr.Var("x"), Expr.Var("y")) -> "d"),
    )
    assertEquals(
      ops.parse("123 OR abc -> def"),
      Right("", Expr.Or(Expr.Emit(UInt16(123)), Expr.Var("abc")) -> "def"),
    )
    assertEquals(
      ops.parse("123 LSHIFT 2 -> def"),
      Right("", Expr.LShift(Expr.Emit(UInt16(123)), UInt16(2)) -> "def"),
    )
    assertEquals(
      ops.parse("123 RSHIFT 2 -> def"),
      Right("", Expr.RShift(Expr.Emit(UInt16(123)), UInt16(2)) -> "def"),
    )
    assertEquals(
      ops.parse("NOT abc -> def"),
      Right("", Expr.Not(Expr.Var("abc")) -> "def"),
    )

  test("parseToMap"):
    val input =
      """123 -> x
        |456 -> y
        |x AND y -> d
        |x OR y -> e
        |x LSHIFT 2 -> f
        |y RSHIFT 2 -> g
        |NOT x -> h
        |NOT y -> i""".stripMargin.linesIterator.toVector

    val expected = Map(
      "x" -> Expr.Emit(UInt16(123)),
      "y" -> Expr.Emit(UInt16(456)),
      "d" -> Expr.And(Expr.Var("x"), Expr.Var("y")),
      "e" -> Expr.Or(Expr.Var("x"), Expr.Var("y")),
      "f" -> Expr.LShift(Expr.Var("x"), UInt16(2)),
      "g" -> Expr.RShift(Expr.Var("y"), UInt16(2)),
      "h" -> Expr.Not(Expr.Var("x")),
      "i" -> Expr.Not(Expr.Var("y")),
    )

    assertEquals(parseToMap(input), expected)
