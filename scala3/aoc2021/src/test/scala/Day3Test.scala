import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day3Test:
  val sample =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day3.parse andThen Day3.part1
    assertEquals(198, solve(sample))
    assertEquals(3985686, solve(Source.fromResource("day3").mkString))

  @Test def t2(): Unit =
    val solve = Day3.parse andThen Day3.part2
    assertEquals(230, solve(sample))
    assertEquals(2555739, solve(Source.fromResource("day3").mkString))
