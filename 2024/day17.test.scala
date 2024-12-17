package `2024`.day17

import munit.FunSuite
import scala.collection.immutable.Queue
import prelude.dedent

class ComputerTest extends FunSuite:
  test("{C:9} <- 2,6"):
    val c = Computer(Regs(0, 0, 9), parse("2,6"))
    assertEquals(c.next.map(_.regs), Some(Regs(0, 1, 9)))

  test("{A:10} <- 5,0,5,1,5,4"):
    val c = Computer(Regs(10, 0, 0), parse("5,0,5,1,5,4"))
    assertEquals(c.run.output, Queue(0, 1, 2))

  test("{A:2024} <- 0,1,5,4,3,0"):
    val c = Computer(Regs(2024, 0, 0), parse("0,1,5,4,3,0"))
    val after = c.run
    assertEquals(after.output, Queue(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0))
    assertEquals(after.regs, Regs(0, 0, 0))

  test("{B:29} <- 1,7"):
    val c = Computer(Regs(0, 29, 0), parse("1,7"))
    assertEquals(c.run.regs, Regs(0, 26, 0))

  test("{B:2024, C:43690} <- 4,0"):
    val c = Computer(Regs(0, 2024, 43690), parse("4,0"))
    assertEquals(c.run.regs, Regs(0, 44354, 43690))

  test("example"):
    val example = """
      Register A: 729
      Register B: 0
      Register C: 0

      Program: 0,1,5,4,3,0
    """.dedent
    val c = Computer.parse(example)
    assertEquals(c.run.output.mkString(","), "4,6,3,5,6,3,5,2,1,0")
