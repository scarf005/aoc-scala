package y2015.day15

import prelude.*

case class Ingredient(
  capacity: Int,
  durability: Int,
  flavor: Int,
  texture: Int,
  calories: Int,
):
  def +(other: Ingredient) = Ingredient(
    capacity = capacity + other.capacity,
    durability = durability + other.durability,
    flavor = flavor + other.flavor,
    texture = texture + other.texture,
    calories = calories + other.calories,
  )
  def *(n: Int) = Ingredient(
    capacity = capacity * n,
    durability = durability * n,
    flavor = flavor * n,
    texture = texture * n,
    calories = calories * n,
  )
  def score =
    Seq(capacity, durability, flavor, texture).map(_.max(0)).product

object Ingredients:
  def apply(ingredients: (String, Ingredient)*): Ingredients =
    Ingredients(ingredients.toMap)

val total = 3

def inc(xs: List[Int]): List[Int] = xs match
  case Nil                   => Nil
  case x :: xs if x == total => 0 :: inc(xs)
  case x :: xs               => (x + 1) :: xs

def combos =
  val init = List(0, 0)

  val generator = Iterator.iterate(inc(init))(inc).find(_.sum == total)

  // take
  // val comboList: LazyList[List[Int]] = LazyList.iterate()

case class Ingredients(ingredients: Map[String, Ingredient]) {}

// def inc(xs: Option[List[Int]]): Option[List[Int]] = xs match
//   case None                         => None
//   case Some(Nil)                    => None
//   case Some(x :: Nil) if x == total => None
//   // case x :: xs if x == total  => x :: inc(xs)
//   // case x :: xs                => (x + 1) :: xs
//   case Some(x :: xs) if x == total => inc(Some(xs)).map(0 :: _)
//   case Some(x :: xs)               => Some((x + 1) :: xs)

// def combos =
//   LazyList
//     .iterate(
//       Some(
//         // List.fill(ingredients.size)(0)
//         List(0, 0, 0),
//       ),
//     )(inc)
//     // .filter(x => x match {
//     //   case None => false
//     //   case Some(xs) => xs.sum == total
//     // })
//     .take(10)
//     // .takeWhile(_.nonEmpty)
//     // .map(_.get)
//     .toList

// def recipes =
//   combos.map { combo =>
//     ingredients.keys
//       .zip(combo)
//       .map { (name, n) => ingredients(name) * n }
//       .reduce { _ + _ }
//   }

val ingredients = Map(
  "Sprinkles" -> Ingredient(
    capacity = 2,
    durability = 0,
    flavor = -2,
    texture = 0,
    calories = 3,
  ),
  "Butterscotch" -> Ingredient(
    capacity = 0,
    durability = 5,
    flavor = -3,
    texture = 0,
    calories = 3,
  ),
  "Chocolate" -> Ingredient(
    capacity = 0,
    durability = 0,
    flavor = 5,
    texture = -1,
    calories = 8,
  ),
  "Candy" -> Ingredient(
    capacity = 0,
    durability = -1,
    flavor = 0,
    texture = 5,
    calories = 8,
  ),
)

def part1(xs: Iterable[Ingredient]): Int =
  xs.map(_.score).max

def part2(xs: Iterable[Ingredient]): Int =
  xs.filter(_.calories == 500).pipe(part1)

// @main def main() =
//   val res = Ingredients(ingredients).combos

//   println(res)
// println(part1(res))
// println(part2(res))
