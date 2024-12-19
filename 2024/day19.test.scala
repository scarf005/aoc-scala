package `2024`.day19

import munit.FunSuite

class Test extends FunSuite:
  test("example"):
    val towel = Towel(Set("r", "wr", "b", "g", "bwu", "rb", "gb", "br"))

    val cases = Map[String, Long](
      "brwrr" -> 2,
      "bggr" -> 1,
      "gbbr" -> 4,
      "rrbgbr" -> 6,
      "ubwu" -> 0,
      "bwurrg" -> 1,
      "brgr" -> 2,
      "bbrgwb" -> 0,
    )
    cases.foreach((a, b) => assertEquals(towel.combos(a), b))
