package prelude.parser

import cats.parse.Parser
import cats.parse.Parser.string
import cats.parse.Rfc5234.{digit, alpha, sp}

/** Parses a sequence of digits into an Int.
  *
  * Example:
  * {{{
  * number.parse("123")    // Right(("", 123))
  * number.parse("12.3")   // Right((".3", 12))
  * number.parse("abc")    // Left(...)
  * }}}
  */
val number: Parser[Int] =
  digit.rep.string.map(_.toInt)

/** Parses a sequence of alphabetic characters.
  *
  * Example:
  * {{{
  * alphas.parse("hello")     // Right(("", "hello"))
  * alphas.parse("hello123")  // Right(("123", "hello"))
  * alphas.parse("123")       // Left(...)
  * }}}
  */
val alphas: Parser[String] =
  alpha.rep.string

/** Creates a parser for a keyword surrounded by optional whitespace.
  *
  * Example:
  * {{{
  * val blue = keyword("blue")
  * blue.parse("blue")      // Right(("", ()))
  * blue.parse(" blue ")    // Right(("", ()))
  * blue.parse("green")     // Left(...)
  * }}}
  */
def keyword(s: String): Parser[Unit] = string(s).surroundedBy(sp.rep0)
