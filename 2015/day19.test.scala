package `2015`.day19

import prelude.*
import munit.FunSuite

class Part1 extends FunSuite:
  test("char"):
    val replacements = Map(
      "H" -> Set("HO", "OH"),
      "O" -> Set("HH"),
    )
    assertEquals(
      "HOH".replaceMolecules(replacements),
      Set("HOOH", "HOHO", "OHOH", "HOOH", "HHHH"),
    )

  test("string"):
    val replacements = Map(
      "HO" -> Set("!!!"),
      "H" -> Set("HO", "OH"),
      "O" -> Set("HH"),
    )
    assertEquals(
      "HOH".replaceMolecules(replacements),
      Set("!!!H", "HOOH", "HOHO", "OHOH", "HOOH", "HHHH"),
    )

  test("do not consider surrounding characters"):
    assertEquals("H2O".replaceMolecules(Map("H" -> Set("OO"))), Set("OO2O"))

class Part2 extends FunSuite:
  test("example"):
    val replacements = Map(
      "e" -> Set("H", "O"),
      "H" -> Set("HO", "OH"),
      "O" -> Set("HH"),
    )

    assertEquals(part2("HOH", replacements), 3)
    assertEquals(part2("HOHOHO", replacements), 6)
