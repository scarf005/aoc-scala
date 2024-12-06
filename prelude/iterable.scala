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
  inline def sumBy[B >: Int](f: A => B)(using Numeric[B]): B = xs.iterator.map(f).sum
