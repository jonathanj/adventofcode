import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day2Test:
  val sample =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day2.parse andThen Day2.part1
    assertEquals(150, solve(sample))
    assertEquals(1635930, solve(Source.fromResource("day2").mkString))

  @Test def t2(): Unit =
    val solve = Day2.parse andThen Day2.part2
    assertEquals(900, solve(sample))
    assertEquals(1781819478, solve(Source.fromResource("day2").mkString))
