import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day11Test:
  val sample =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day11.parse andThen Day11.part1
    assertEquals(1656, solve(sample))
    assertEquals(1675, solve(Source.fromResource("day11").mkString))

  @Test def t2(): Unit =
    val solve = Day11.parse andThen Day11.part2
    assertEquals(195, solve(sample))
    assertEquals(515, solve(Source.fromResource("day11").mkString))
