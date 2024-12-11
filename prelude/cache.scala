package prelude

def memoize[A, B](f: A => B): A => B =
  val cache = collection.mutable.HashMap.empty[A, B]
  a => cache.getOrElseUpdate(a, f(a))
