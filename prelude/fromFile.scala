package prelude

import scala.io.Source.fromFile
import scala.io.BufferedSource

export scala.io.Source.fromFile

/** pass `this` to get package name */
def getPackageName(pkg: Any) =
  pkg.getClass.getPackageName

/** read input from file, package `s"$year.day$day"` maps to
  * `s".cache/$year/$day.txt"`
  */
def readInput(pkg: Any): BufferedSource = getPackageName(pkg) match
  case s"$year.day$day" => fromFile(s".cache/$year/$day.txt")
  case pkg => throw IllegalArgumentException(s"Invalid name format $pkg")
