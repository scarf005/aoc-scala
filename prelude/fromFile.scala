package prelude

import scala.io.Source.fromFile
import scala.io.BufferedSource

export scala.io.Source.fromFile

/** Gets the package name from a class instance.
  *
  * Example:
  * {{{
  * // In file src/main/scala/2024/day01.scala:
  * package `2024`.day01
  *
  * @main def main() =
  *   println(getPackageName(this))  // "2024.day01"
  * }}}
  */
def getPackageName(pkg: Any) =
  pkg.getClass.getPackageName

/** Reads input file for Advent of Code solutions. Maps package name to input
  * file location:
  *   - Package "2024.day01" reads from ".cache/2024/1.txt"
  *   - Package "2015.day25" reads from ".cache/2015/25.txt"
  *
  * Example:
  * {{{
  * // In file src/main/scala/2024/day01.scala:
  * package `2024`.day01
  *
  * val input = readInput(this).getLines.toVector
  * }}}
  */
def readInput(pkg: Any): BufferedSource = getPackageName(pkg) match
  case s"$year.day$day" => fromFile(s".cache/$year/$day.txt")
  case pkg => throw IllegalArgumentException(s"Invalid name format $pkg")
