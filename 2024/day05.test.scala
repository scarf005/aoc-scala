package `2024`.day05

import prelude.*
import munit.FunSuite

val example =
  """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

class Test extends FunSuite:
  val (deps, pages) = example |> parse
  val (wrongs, rights) = pages.partitionBySort(using deps)

  test("part 1"):
    assertEquals(rights.sumBy(middle), 143)

  test("part 2"):
    assertEquals(wrongs.sumBy(middle), 123)
