import munit.FunSuite
import y2015.day6.*
import utils.Point

class Day6Tests extends FunSuite:
  test("action"):
    assertEquals(action.parse("turn on"), Right(value = ("", Action.On)))
    assertEquals(action.parse("turn off"), Right(value = ("", Action.Off)))
    assertEquals(action.parse("toggle"), Right(value = ("", Action.Toggle)))

  test("coord"):
    assertEquals(coord.parse("123,456"), Right(value = ("", Point(123, 456))))

  test("selection"):
    assertEquals(
      selection.parse("0,0 through 999,999"),
      Right(value = ("", Selection(Point(0, 0), Point(999, 999)))),
    )

  test("instruction"):
    assertEquals(
      instruction.parse("turn on 0,0 through 999,999"),
      Right(value =
        ("", Instruction(Action.On, Selection(Point(0, 0), Point(999, 999)))),
      ),
    )
