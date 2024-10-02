package `2015`.day05

import munit.FunSuite

class Day5Tests extends FunSuite:
  test("Part1"):
    assertEquals(part1("ugknbfddgicrmopn"), true)
    assertEquals(part1("aaa"), true)
    assertEquals(part1("jchzalrnumimnmhp"), false)
    assertEquals(part1("haegwjzuvuyypxyu"), false)
    assertEquals(part1("dvszwmarrgswjxmb"), false)

  test("Part2"):
    assertEquals(part2("qjhvhtzxzqqjkmpb"), true)
    assertEquals(part2("xxyxx"), true)
    assertEquals(part2("uurcxstgmygtbstg"), false)
    assertEquals(part2("ieodomkazucvgmuy"), false)
