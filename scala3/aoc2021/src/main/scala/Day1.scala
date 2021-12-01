import aoc2021.Util

object Day1:
  def parse = Util.stringLinesToLongs

  def part1(input: Seq[Long]) =
    val initial = (0L, input.head)
    input.foldLeft(initial) { case ((cnt, prev), n) =>
      (cnt + (if (n > prev) 1 else 0), n)
    }._1

  def part2(input: Seq[Long]) =
    part1(input.sliding(3, 1).map(_.sum).toSeq)
