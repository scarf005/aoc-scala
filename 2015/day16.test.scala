package y2015.day16

import prelude.*
import munit.FunSuite
import y2015.day16.*

val auntRight = Aunt(1, Map("goldfish" -> 9, "cars" -> 0, "samoyeds" -> 9))
val auntEmpty = Aunt(1, Map.empty)

class ParserTests extends FunSuite:
  assertEquals(
    aunt.parseAll("Sue 1: goldfish: 9, cars: 0, samoyeds: 9"),
    Right(auntRight),
  )

class MatchEqTests extends FunSuite:
  val clue = Map("goldfish" -> 9, "cars" -> 0, "samoyeds" -> 9)

  test("empty"):
    assertEquals(auntEmpty.matchEq(clue), true)

  test("matches"):
    assertEquals(auntRight.matchEq(clue), true)

  test("mismatches"):
    assertEquals(auntRight.matchEq(clue + ("goldfish" -> 10)), false)

  test("unknown clue"):
    assertEquals(auntRight.matchEq(clue + ("unknown" -> 10)), true)

class MatchRangeTests extends FunSuite:
  val auntRight =
    Aunt(1, Map("cats" -> 6, "trees" -> 6, "pomeranians" -> 4, "goldfish" -> 4))

  val clue =
    Map("cats" -> 5, "trees" -> 5, "pomeranians" -> 5, "goldfish" -> 5)

  test("empty"):
    assertEquals(auntEmpty.matchRange(clue), true)

  test("matches"):
    assertEquals(auntRight.matchRange(clue), true)

  test("cats"):
    assertEquals(auntRight.matchRange(clue + ("cats" -> 10)), false)
    assertEquals(auntRight.matchRange(clue + ("cats" -> 5)), true)

  test("trees"):
    assertEquals(auntRight.matchRange(clue + ("trees" -> 10)), false)
    assertEquals(auntRight.matchRange(clue + ("trees" -> 5)), true)

  test("pomeranians"):
    assertEquals(auntRight.matchRange(clue + ("pomeranians" -> 10)), true)
    assertEquals(auntRight.matchRange(clue + ("pomeranians" -> 4)), false)

  test("goldfish"):
    assertEquals(auntRight.matchRange(clue + ("goldfish" -> 10)), true)
    assertEquals(auntRight.matchRange(clue + ("goldfish" -> 4)), false)

  test("unknown clue"):
    assertEquals(auntRight.matchRange(clue + ("unknown" -> 10)), true)
