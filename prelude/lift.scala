package prelude

extension (a: Boolean) inline def toInt = if a then 1 else 0

extension [A](a: A)
  /** more conveient syntax sugar for lifting partial functions to
    * [[scala.Option]]
    */
  inline def lift[B](f: PartialFunction[A, B]) = f.lift(a)

extension [A, B](x: (Option[A], Option[B]))
  def bisequence: Option[(A, B)] = for a <- x._1; b <- x._2 yield (a, b)
