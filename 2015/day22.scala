package `2015`.day22

import prelude.*

case class Magic(
  name: String,
  mana: Int = 0,
  dmg: Int = 0,
  hp: Int = 0,
  ac: Int = 0,
  turns: Int = 0,
)

// Magic Missile costs 53 mana. It instantly does 4 damage.
// Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
// Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
// Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
// Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.

val magics = Set(
  Magic("Magic Missile", mana = 53, dmg = 4),
  Magic("Drain", mana = 73, dmg = 2, hp = 2),
  Magic("Shield", mana = 113, ac = 7, turns = 6),
  Magic("Poison", mana = 173, turns = 6),
  Magic("Recharge", mana = 229, turns = 5),
)
