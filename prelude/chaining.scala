package prelude

extension [A](self: A)
  /** Applies `f` to the value for its side effects, and returns the original
    * value.
    *
    * {{{
    *   scala> import prelude.*
    *
    *   scala> val xs = List(1, 2, 3).tap(ys => println("debug " + ys.toString))
    *   debug List(1, 2, 3)
    *   xs: List[Int] = List(1, 2, 3)
    * }}}
    *
    * @param f
    *   the function to apply to the value.
    * @tparam B
    *   the result type of the function `f`.
    * @return
    *   the original value `self`.
    */
  inline def tap[B](f: A => B): A = { f(self); self }

  /** Converts the value by applying the function `f`.
    *
    * {{{
    *   scala> import prelude.*
    *
    *   scala> val times6 = (_: Int) * 6
    *   times6: Int => Int = \$\$Lambda\$2023/975629453@17143b3b
    *
    *   scala> val i = (1 - 2 - 3).pipe(times6).pipe(scala.math.abs)
    *   i: Int = 24
    * }}}
    *
    * @param f
    *   the function to apply to the value.
    * @tparam B
    *   the result type of the function `f`.
    * @return
    *   a new value resulting from applying the given function `f` to this
    *   value.
    */
  inline def pipe[B](f: A => B): B = f(self)

  /** Alias for pipe. Provides F#-style forward pipe operator.
    */
  inline def |>[B](f: A => B): B = f(self)
