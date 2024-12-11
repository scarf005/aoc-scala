package prelude

inline def time(f: => Unit) = {
  val begin = System.currentTimeMillis()
  f
  val end = System.currentTimeMillis()
  println(s"Elapsed time: ${end - begin}ms")
}
