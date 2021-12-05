import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day5Test:
  val sample =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day5.parse andThen Day5.part1
    assertEquals(5, solve(sample))
    assertEquals(5169, solve(Source.fromResource("day5").mkString))

  @Test def t2(): Unit =
    val solve = Day5.parse andThen Day5.part2
    assertEquals(12, solve(sample))
    assertEquals(22083, solve(Source.fromResource("day5").mkString))
