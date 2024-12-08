package `2015`.day25

def grid(row: Int, col: Int): Int =
  (row + col - 1) * (row + col - 2) / 2 + col

def next(n: Long): Long = n * 252533 % 33554393

def at(n: Long) = Iterator.iterate(20151125L)(next).drop(n.toInt - 1).next

@main def main() =
  val target = grid(row = 2981, col = 3075)
  println(at(target))
