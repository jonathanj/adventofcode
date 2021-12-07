import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day7Test:
  val sample = "16,1,2,0,4,2,7,1,2,14"

  @Test def t1(): Unit =
    val solve = Day7.parse andThen Day7.part1
    assertEquals(37, solve(sample))
    // Not 343
    assertEquals(340987, solve(Source.fromResource("day7").mkString))

  @Test def t2(): Unit =
    val solve = Day7.parse andThen Day7.part2
    assertEquals(168, solve(sample))
    assertEquals(96987874, solve(Source.fromResource("day7").mkString))
