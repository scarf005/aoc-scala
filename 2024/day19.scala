package `2024`.day19

import scala.collection.concurrent.TrieMap
import prelude.*

case class Towel(val designs: Set[String]):
  def combos(towel: String): Long =
    val memo = TrieMap[String, Long]("" -> 1)

    def go(cur: String): Long = memo.getOrElseUpdate(
      cur,
      designs.view.filter(cur.startsWith).sumBy { p => go(cur.drop(p.size)) },
    )
    go(towel)

@main def main =
  val input = fromFile(".cache/2024/19.txt").getLines
  val (towel, cases) = input.splitAt(2) |> ((a, b) =>
    (a.mkString.split(",").map(_.trim).toSet |> Towel.apply, b.toVector)
  )
  val result = cases.map(towel.combos)
  println(s"part1: ${result.count(_ > 0)}")
  println(s"part2: ${result.sum}")
