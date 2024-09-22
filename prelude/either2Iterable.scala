package prelude

import scala.language.implicitConversions

/** flatMap [[scala.Either]] */
implicit def either2Iterable[A](e: Either[?, A]): Iterable[A] =
  e.toOption.toSeq
