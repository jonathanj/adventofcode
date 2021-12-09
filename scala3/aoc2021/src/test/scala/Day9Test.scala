import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day9Test:
  val sample =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day9.parse andThen Day9.part1
    assertEquals(15, solve(sample))
    assertEquals(588, solve(Source.fromResource("day9").mkString))

  @Test def t2(): Unit =
    val solve = Day9.parse andThen Day9.part2
    assertEquals(1134, solve(sample))
    assertEquals(964712, solve(Source.fromResource("day9").mkString))
