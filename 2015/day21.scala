package `2015`.day21

case class Item(name: String, cost: Int, dmg: Int = 0, ac: Int = 0):
  override def toString = name

val weapons = Set(
  Item("Dagger", cost = 8, dmg = 4),
  Item("Shortsword", cost = 10, dmg = 5),
  Item("Warhammer", cost = 25, dmg = 6),
  Item("Longsword", cost = 40, dmg = 7),
  Item("Greataxe", cost = 74, dmg = 8),
)

val armors = Set(
  Item("No Armor", cost = 0),
  Item("Leather", cost = 13, ac = 1),
  Item("Chainmail", cost = 31, ac = 2),
  Item("Splintmail", cost = 53, ac = 3),
  Item("Bandedmail", cost = 75, ac = 4),
  Item("Platemail", cost = 102, ac = 5),
)

val rings = Set(
  Item("No Left Ring", cost = 0),
  Item("No Right Ring", cost = 0),
  Item("Damage +1", cost = 25, dmg = 1),
  Item("Damage +2", cost = 50, dmg = 2),
  Item("Damage +3", cost = 100, dmg = 3),
  Item("Defense +1", cost = 20, ac = 1),
  Item("Defense +2", cost = 40, ac = 2),
  Item("Defense +3", cost = 80, ac = 3),
)

case class Character(hp: Int, dmg: Int = 0, ac: Int = 0)

extension (c: Character)
  def deals(other: Character) = math.max(c.dmg - other.ac, 1)

  infix def winsAgainst(other: Character): Boolean =
    diesAtTurn(c, other) >= diesAtTurn(other, c)

def diesAtTurn(a: Character, b: Character) =
  (a.hp.toFloat / b.deals(a).toFloat).ceil.toInt

@main def main() =
  val player = Character(hp = 100)
  val boss = Character(hp = 104, dmg = 8, ac = 1)

  val combos = for
    weapon <- weapons
    armor <- armors
    ring1 <- rings
    ring2 <- rings - ring1
    items = Vector(weapon, armor, ring1, ring2)
    v = (field: Item => Int) => items.map(field).sum
    player = Character(hp = 100, v(_.dmg), v(_.ac))
  yield (v(_.cost), player, items)

  val part1 = combos.filter(_._2 winsAgainst boss).minBy(_._1)
  println(part1)

  val part2 = combos.filterNot(_._2 winsAgainst boss).maxBy(_._1)
  println(part2)
