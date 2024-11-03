package `2015`.day21

import prelude.*
import munit.FunSuite

class CharacterTest extends FunSuite:
  test("example"):
    val player = Character(hp = 8, dmg = 5, ac = 5)
    val boss = Character(hp = 12, dmg = 7, ac = 2)

    assert(player winsAgainst boss)
