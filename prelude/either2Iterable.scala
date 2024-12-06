package prelude

import scala.language.implicitConversions

/** Converts Either to Iterable by keeping only the Right value if it exists.
  *
  * Example:
  * {{{
  * val success = Right(42)
  * val failure = Left("error")
  *
  * success.toList      // List(42)
  * failure.toList      // List()
  *
  * List(success, failure).flatten  // List(42)
  * }}}
  */
implicit inline def either2Iterable[A](e: Either[?, A]): Iterable[A] =
  e.toOption.toSeq
