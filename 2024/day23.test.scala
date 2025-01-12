package `2024`.day23

import munit.FunSuite
import prelude.dedent

val example = """
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
""".dedent

class Test extends FunSuite:
  test("example"):
    assertEquals(part1(example), 7)
    assertEquals(part2(example), "co,de,ka,ta")
