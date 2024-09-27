import munit.FunSuite
import y2015.day8.*

class Day8Tests extends FunSuite:
  val xs = Vector(
    """""""" -> (2, 0),
    """"abc"""" -> (5, 3),
    """"aaa\"aaa"""" -> (10, 7),
    """"\x27"""" -> (6, 1),
  )

  xs.foreach { (x, rest) =>
    val (literalLen, memoryLen) = rest
    test(s"part1 [$x]"):
      assertEquals(x.size, literalLen)
      assertEquals(x.memoryLen, memoryLen)
      assertEquals(part1(x), literalLen - memoryLen)
  }

  test("part1"):
    assertEquals(xs.map(_._1).map(part1).sum, 12)

  val ys = Vector(
    """""""" -> (2, 6),
    """"abc"""" -> (5, 9),
    """"aaa\"aaa"""" -> (10, 16),
    """"\x27"""" -> (6, 11),
  )

  ys.foreach { (y, rest) =>
    val (literalLen, encodedLen) = rest
    assertEquals(y.size, literalLen)
    assertEquals(y.encodedLen, encodedLen)
    assertEquals(part2(y), encodedLen - literalLen)
  }

  test("part2"):
    assertEquals(ys.map(_._1).map(part2).sum, 19)
