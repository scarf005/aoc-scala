package prelude.parser

import cats.parse.Parser
import cats.parse.Parser.string
import cats.parse.Rfc5234.{digit, alpha, sp}

val number: Parser[Int] =
  digit.rep.string.map(_.toInt)

val alphas: Parser[String] =
  alpha.rep.string

def keyword(s: String): Parser[Unit] = string(s).surroundedBy(sp.rep0)
