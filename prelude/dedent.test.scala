package prelude
import munit.FunSuite

class DedentTest extends FunSuite:
  test("basic"):
    assertEquals(
      d"""
        aaa
        aaa
    """,
      "aaa\naaa",
    )

  test("string interpolator works"):
    assertEquals(
      d"""
      aaa
        aaa
      aaa
    """,
      "aaa\n  aaa\naaa",
    )

  test("dedent works"):
    assertEquals(
      """
        aaa
          aaa
        aaa
      """.dedent,
      "aaa\n  aaa\naaa",
    )

  test("last line is trimmed away"):
    assertEquals(
      """
        aaa
          aaa
        aaa""".dedent,
      "aaa\n  aaa\naaa",
    )
