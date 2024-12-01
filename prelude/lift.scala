package prelude

extension (a: Boolean) inline def toInt = if a then 1 else 0

extension [A](a: A)
  /** More convenient syntax sugar for lifting partial functions to
    * [[scala.Option]]
    *
    * Example:
    * {{{
    * val parsePositive: PartialFunction[String, Int] = {
    *   case s if s.matches("\\d+") && s.toInt > 0 => s.toInt
    * }
    *
    * "42".lift(parsePositive)     // Some(42)
    * "-1".lift(parsePositive)     // None
    * "abc".lift(parsePositive)    // None
    * }}}
    */
  inline def lift[B](f: PartialFunction[A, B]) = f.lift(a)

extension [A, B](x: (Option[A], Option[B]))
  /** Sequences a tuple of Options into an Option of tuple.
    *
    * Example:
    * {{{
    * val a = Some(1)
    * val b = Some("hello")
    * (a, b).bisequence              // Some((1, "hello"))
    *
    * val c = Some(2)
    * val d = None
    * (c, d).bisequence              // None
    * }}}
    */
  def bisequence: Option[(A, B)] = for a <- x._1; b <- x._2 yield (a, b)
