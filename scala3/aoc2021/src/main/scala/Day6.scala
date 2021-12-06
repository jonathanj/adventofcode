import aoc2021.Util

object Day6:
  def parse = Util.separatedInts

  /**
   * Track frequencies of lanternfish, instead of individual lantern fish
   * (which will run out of heap space pretty quickly).
   */
  def solve(days: Int)(input: Seq[Int]) =
    val initial = Map(0 -> 0L, 1 -> 0L, 2 -> 0L, 3 -> 0L, 4 -> 0L, 5 -> 0L, 6 -> 0L, 7 -> 0L, 8 -> 0L, 9 -> 0L)
    def inc(v: Long)(mv: Option[Long]): Option[Long] = mv.orElse(Some(0L)).map(_ + v)
    Stream.iterate(Util.frequencies(input)) { fishes =>
      fishes.foldLeft(initial) { case (m, (k, v)) =>
        val f = inc(v)
        if k == 0 then
          m
            .updatedWith(6)(f)
            .updatedWith(8)(f)
        else
          m.updatedWith(k - 1)(f)
      }
    }(days).values.sum

  def part1 = solve(80)

  def part2 = solve(256)
