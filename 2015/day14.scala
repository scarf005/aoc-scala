package y2015.day14

import prelude.*

case class Deer(speed: Int, flyTime: Int, restTime: Int):
  def flightDistance(time: Int) =
    val cycle = flyTime + restTime
    val (full, partial) = time / cycle -> time % cycle
    full * flyTime * speed + (partial min flyTime) * speed

def parse(input: String) = input match
  case s"$reindeer can fly $speed km/s for $flyTime seconds, but then must rest for $restTime seconds." =>
    reindeer -> Deer(speed.toInt, flyTime.toInt, restTime.toInt)

def part1(deers: Iterable[Deer], time: Int) =
  deers.map(_.flightDistance(time)).max

def part2(deers: Map[String, Deer], time: Int) =
  val points = deers.keys.map(_ -> 0).to(scala.collection.mutable.Map)
  (1 to time).foreach { t =>
    val distances = deers.view.mapValues(_.flightDistance(t))
    val maxDistance = distances.maxBy(_._2)._2
    distances
      .filter(_._2 == maxDistance)
      .foreach { (name, d) => points(name) += 1 }
  }
  points.values.max

@main def main() =
  val deers = fromFile(".cache/2015/14.txt").getLines.map(parse).toMap
  println(part1(deers.values, 2503))
  println(part2(deers, 2503))
