import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day24Test:
  @Test def t1(): Unit =
    val solve = Day24.parse andThen Day24.part1
    assertEquals("91297395919993", solve(Source.fromResource("day24").mkString))

  @Test def t2(): Unit =
    val solve = Day24.parse andThen Day24.part2
    assertEquals("71131151917891", solve(Source.fromResource("day24").mkString))
