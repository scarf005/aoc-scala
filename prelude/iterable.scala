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
  inline def sumBy[B](f: A => B)(using num: Numeric[B]): B =
    xs.iterator.foldLeft(num.zero)((acc, x) => num.plus(acc, f(x)))

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

extension [A](xs: Iterator[A])
  /** Splits the iterator into `n` shards, cycling elements between them.
    *
    * Example
    * ```
    * Iterator.from(0).shard(3)
    * //=> List(
    *   Iterator(0, 3, 6, ...),
    *   Iterator(1, 4, 7, ...),
    *   Iterator(2, 5, 8, ...)
    *  )
    * ```
    */
  def shard(n: Int): List[Iterator[A]] =
    require(n > 0, "Number of shards must be positive")

    val iterators = List.fill(n)(collection.mutable.Queue[A]())

    def shardIterator(index: Int): Iterator[A] = new Iterator[A] {
      override def hasNext: Boolean = xs.hasNext || iterators(index).nonEmpty

      override def next(): A =
        if (iterators(index).isEmpty && xs.hasNext) {
          // Distribute the next batch of elements to their respective shard queues
          for (i <- 0 until n if xs.hasNext) { iterators(i).enqueue(xs.next()) }
        }
        if (iterators(index).nonEmpty) {
          iterators(index).dequeue()
        } else {
          throw new NoSuchElementException("next on empty iterator")
        }
    }

    List.tabulate(n)(shardIterator)
