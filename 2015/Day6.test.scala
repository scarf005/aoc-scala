import munit.FunSuite
import day6.*
import util.Point

class Day6Tests extends FunSuite:
  test("Parser"):
    assertEquals(Parser.parse(Parser.action, "turn on").get, Action.On)
    assertEquals(Parser.parse(Parser.action, "turn off").get, Action.Off)
    assertEquals(Parser.parse(Parser.action, "toggle").get, Action.Toggle)

    assertEquals(Parser.parse(Parser.coord, "123,456").get, Point(123, 456))
    assertEquals(
      Parser.parse(Parser.instruction, "turn on 0,0 through 999,999").get,
      Instruction(Action.On, Selection(Point(0, 0), Point(999, 999))),
    )

  // test("Part1"):
  //   assertEquals(part1("ugknbfddgicrmopn"), true)
  //   assertEquals(part1("aaa"), true)
  //   assertEquals(part1("jchzalrnumimnmhp"), false)
  //   assertEquals(part1("haegwjzuvuyypxyu"), false)
  //   assertEquals(part1("dvszwmarrgswjxmb"), false)

  // test("Part2"):
  //   assertEquals(part2("qjhvhtzxzqqjkmpb"), true)
  //   assertEquals(part2("xxyxx"), true)
  //   assertEquals(part2("uurcxstgmygtbstg"), false)
  //   assertEquals(part2("ieodomkazucvgmuy"), false)
