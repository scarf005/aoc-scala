package prelude

import scala.annotation.targetName
import scala.collection.generic.IsSeq

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

extension [A](x: Seq[A])
  def combinationsRepeating(n: Int): Iterator[Seq[A]] =
    x.zipWithIndex.combinations(n).map(_.map(_._1))

  @targetName("combinations2")
  inline def combinationsN(n: 2): Iterator[(A, A)] =
    x.combinationsRepeating(n).map { case Seq(a, b) => (a, b) }
  @targetName("combinations3")
  inline def combinationsN(n: 3): Iterator[(A, A, A)] =
    x.combinationsRepeating(n).map { case Seq(a, b, c) => (a, b, c) }
  @targetName("combinations4")
  inline def combinationsN(n: 4): Iterator[(A, A, A, A)] =
    x.combinationsRepeating(n).map { case Seq(a, b, c, d) => (a, b, c, d) }

extension [A](it: Iterator[A])
  /** Takes longest prefix of elements that does not satisfy a predicate.
    * $orderDependent
    * @param p
    *   The predicate used to test elements.
    * @return
    *   the longest prefix of this $coll whose elements all do not satisfy the
    *   predicate `p`.
    */
  def takeUntil(p: A => Boolean): Iterator[A] =
    new Iterator[A]:
      private var done = false
      def hasNext = !done && it.hasNext
      def next() =
        val x = it.next()
        if p(x) then done = true
        x

extension [A](xs: Iterable[A])
  def frequencies: Map[A, Int] =
    xs.groupMapReduce(identity)(_ => 1)(_ + _)
  def frequenciesL: Map[A, Long] =
    xs.groupMapReduce(identity)(_ => 1L)(_ + _)

extension [C](c: C)(using isSeq: IsSeq[C])
  def dropBothWhile(p: isSeq.A => Boolean): isSeq.C =
    val xs = isSeq(c)
    val leftIndex = xs.indexWhere(!p(_))
    val rightIndex = xs.lastIndexWhere(!p(_))
    xs.slice(leftIndex, rightIndex + 1)
