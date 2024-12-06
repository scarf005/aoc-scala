package `2015`.day06

import munit.FunSuite
import utils.Pos

class Day6Tests extends FunSuite:
  test("action"):
    assertEquals(action.parse("turn on"), Right(value = ("", Action.On)))
    assertEquals(action.parse("turn off"), Right(value = ("", Action.Off)))
    assertEquals(action.parse("toggle"), Right(value = ("", Action.Toggle)))

  test("coord"):
    assertEquals(coord.parse("123,456"), Right(value = ("", Pos(123, 456))))

  test("selection"):
    assertEquals(
      selection.parse("0,0 through 999,999"),
      Right(value = ("", Selection(Pos(0, 0), Pos(999, 999)))),
    )

  test("instruction"):
    assertEquals(
      instruction.parse("turn on 0,0 through 999,999"),
      Right(value =
        ("", Instruction(Action.On, Selection(Pos(0, 0), Pos(999, 999)))),
      ),
    )
