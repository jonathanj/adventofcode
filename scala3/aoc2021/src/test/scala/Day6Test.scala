import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day6Test:
  val sample = "3,4,3,1,2"

  @Test def t1(): Unit =
    val solve = Day6.parse andThen Day6.part1
    assertEquals(5934, solve(sample))
    assertEquals(393019, solve(Source.fromResource("day6").mkString))

  @Test def t2(): Unit =
    val solve = Day6.parse andThen Day6.part2
    assertEquals(26984457539L, solve(sample))
    assertEquals(1757714216975L, solve(Source.fromResource("day6").mkString))
