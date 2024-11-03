package `2015`.day20

def part1(n: Int): Int =
  val size = n / 10
  val arr = Array.fill(size)(10)
  for i <- 2 until size do for j <- i until size by i do arr(j) += i * 10
  arr.indexWhere(_ >= n)

def part2(n: Int): Int =
  val size = n / 10
  val arr = Array.fill(size)(0)
  for i <- 1 until size do
    for j <- i until math.min(i * 50, size) by i do arr(j) += i * 11
  arr.indexWhere(_ >= n)

@main def main() =
  val input = 33100000

  println(part1(input))
  println(part2(input))
