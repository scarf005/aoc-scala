package prelude.parser

import cats.parse.Parser
import cats.parse.Rfc5234.{digit, alpha}

val number: Parser[Int] =
  digit.rep.string.map(_.toInt)

val alphas: Parser[String] =
  alpha.rep.string
