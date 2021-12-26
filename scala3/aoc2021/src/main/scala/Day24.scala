import aoc2021.Util.*

object Day24:
  type Input = Map[Int, (Int, Int)]

  // This puzzle could mostly be solved on paper by reading (and reverse engineering)
  // the input puzzle, and creating a spreadsheet of digit ranges that will satisfy
  // the algorithm and produce zero.
  def parse(input: String): Input =
    def constant(s: String) = s.split(' ').last.toInt
    stringLines(input).grouped(18)
      // Extract the "add x _" and "add y _" constants
      .map { lines => (constant(lines(5)), constant(lines(15))) }
      .zipWithIndex
      .toList
      // In order for an input to result in a zero value, some digits need to be set to a number that modulo 26 plus C
      // will be zero. Not all digits can be affected so keep track of the ones that can be adjusted.
      .foldLeft((List[(Int, Int)]()), Map[Int, (Int, Int)]()) { case ((stack, adjustments), ((a, b), i)) =>
        if a > 0 then
          ((i, b) :: stack, adjustments)
        else
          val (j, bj) = stack.head
          (stack.tail, adjustments + (i -> (j, bj + a)))
      }._2

  def solve(f: (Int, Int) => Int, n: Int)(adjustments: Input) =
    adjustments.foldLeft(Map[Int, Int]()) { case (numbers, (i, (j, delta))) =>
      numbers
        + (i -> f(n, n + delta))
        + (j -> f(n, n - delta))
    }.toList.sorted.map(_._2).mkString

  def part1 = solve(Math.min, 9)

  def part2 = solve(Math.max, 1)
