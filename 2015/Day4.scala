package day4

import scala.util.chaining.*
import scala.io.Source.fromFile
import java.security.MessageDigest
import cats.effect.IO
import cats.effect.unsafe.implicits.global

def md5(s: String) =
  MessageDigest
    .getInstance("MD5")
    .digest(s.getBytes)
    .map("%02x".format(_))
    .mkString

def solve(n: Int)(input: String) =
  val cores = Runtime.getRuntime.availableProcessors
  val zeros = "0" * n

  fs2.Stream
    .iterate(0)(_ + 1)
    .chunkN(10_000)
    .parEvalMap(cores) { _.map(x => (x, md5(s"$input$x"))).pipe(IO.pure) }
    .unchunks
    .find((x, hashed) => hashed.startsWith(zeros))
    .compile
    .onlyOrError
    .unsafeRunSync()
    ._1

def part1 = solve(n = 5)
def part2 = solve(n = 6)

@main def main() =
  val input = fromFile(".cache/04.txt").mkString.trim

  println(part1(input))
  println(part2(input))
