package prelude
import munit.FunSuite

class IteratorShardTest extends FunSuite:
  test("shard splits an infinite iterator correctly"):
    val iterator = Iterator.from(0)
    val shards = iterator.shard(3)

    val shard0 = shards(0).take(5).toList
    val shard1 = shards(1).take(5).toList
    val shard2 = shards(2).take(5).toList

    assertEquals(shard0, List(0, 3, 6, 9, 12))
    assertEquals(shard1, List(1, 4, 7, 10, 13))
    assertEquals(shard2, List(2, 5, 8, 11, 14))

  test("shard handles empty iterator"):
    val iterator = Iterator.empty[Int]
    val shards = iterator.shard(3)

    assertEquals(shards.map(_.isEmpty), List(true, true, true))

  test("shard throws exception for non-positive shard count"):
    intercept[IllegalArgumentException] {
      Iterator.from(0).shard(0)
    }
