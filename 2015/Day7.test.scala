import munit.FunSuite
import day7.*
import scala.collection.mutable.HashMap
import utils.uint.*
import scala.util.chaining.*

class Day7Tests extends FunSuite:
  test("Parser"):
    import Parser.*

    assertEquals(parse(number, "123").get, UInt16(123))
    assertEquals(parse(signal, "123").get, Expr.Emit(UInt16(123)))
    assertEquals(parse(wire, "abc").get, Expr.Var("abc"))

    assertEquals(
      parse(connection, "123 -> abc").get,
      "abc" -> Expr.Emit(UInt16(123)),
    )
    assertEquals(
      parse(and, "abc AND def -> ghi").get,
      "ghi" -> Expr.And(Expr.Var("abc"), Expr.Var("def")),
    )
    assertEquals(
      parse(or, "abc OR def -> ghi").get,
      "ghi" -> Expr.Or(Expr.Var("abc"), Expr.Var("def")),
    )
    assertEquals(
      parse(lshift, "abc LSHIFT 2 -> def").get,
      "def" -> Expr.LShift(Expr.Var("abc"), UInt16(2)),
    )
    assertEquals(
      parse(rshift, "abc RSHIFT 2 -> def").get,
      "def" -> Expr.RShift(Expr.Var("abc"), UInt16(2)),
    )
    assertEquals(
      parse(not, "NOT abc -> def").get,
      "def" -> Expr.Not(Expr.Var("abc")),
    )

  test("Part 1"):
    val input =
      """123 -> x
        |456 -> y
        |x AND y -> d
        |x OR y -> e
        |x LSHIFT 2 -> f
        |y RSHIFT 2 -> g
        |NOT x -> h
        |NOT y -> i""".stripMargin.linesIterator.toVector
        .pipe(Parser.parseAll)

    Vector(
      "d" -> UInt16(72),
      "e" -> UInt16(507),
      "f" -> UInt16(492),
      "g" -> UInt16(114),
      "h" -> UInt16(65412),
      "i" -> UInt16(65079),
      "x" -> UInt16(123),
      "y" -> UInt16(456),
    ).foreach { (name, expected) =>
      assertEquals(solve(input, name), expected)
    }
