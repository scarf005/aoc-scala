import munit.FunSuite
import `2015`.day15.*
import prelude.*

class Day15Tests extends FunSuite:
  val ingredients = Ingredients(
    "Butterscotch" -> Ingredient(
      capacity = -1,
      durability = -2,
      flavor = 6,
      texture = 3,
      calories = 8,
    ),
    "Cinnamon" -> Ingredient(
      capacity = 2,
      durability = 3,
      flavor = -2,
      texture = -1,
      calories = 3,
    ),
  )
  // val res = ingredients.recipes

  // test("Part 1"):
  //   assertEquals(part1(res), 62842880)

  // test("Part 2"):
  //   assertEquals(part2(res), 57600000)
