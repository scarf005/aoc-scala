package prelude

extension [K, A](a: Map[K, A])
  /** Combines two maps by their keys, creating pairs of values for matching
    * keys. Only keys that exist in both maps with defined values will be
    * included in the result.
    *
    * Example:
    * {{{
    * val a = Map(1 -> "one", 2 -> "two", 3 -> "three")
    * val b = Map(1 -> 1.0, 2 -> 2.0, 4 -> 4.0)
    * zipAllByKey(a, b) // Map(1 -> ("one", 1.0), 2 -> ("two", 2.0))
    * }}}
    */
  infix inline def zipByKey[B](b: Map[K, B]): Map[K, (A, B)] =
    for
      (key, aValue) <- a
      bValue <- b.get(key)
    yield key -> (aValue, bValue)
