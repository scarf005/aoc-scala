package prelude

extension [A](xs: IterableOnce[A])
  /** Maps elements to numeric values and sums them.
    *
    * Example:
    * {{{
    * val items = List("a", "bb", "ccc")
    *
    * items.sumBy(_.length)  // 6
    *
    * // equivalent to
    * items.map(_.length).sum  // 6
    * }}}
    */
  inline def sumBy[B >: Int](f: A => B)(using Numeric[B]): B =
    xs.iterator.map(f).sum

extension [C <: Iterable, A](xs: C[A])
  /** Maps the iterable to a new iterable using the given function `f` if the
    * iterable is not empty. Otherwise, returns the given value `default`.
    */
  inline def orEmpty[B](f: C[A] => B)(default: => B) =
    if (xs.isEmpty) default else f(xs)
