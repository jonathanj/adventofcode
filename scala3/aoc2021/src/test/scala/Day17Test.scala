import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day17Test:
  @Test def t1(): Unit =
    val solve = Day17.parse andThen Day17.part1
    assertEquals(45, solve("target area: x=20..30, y=-10..-5"))
    assertEquals(7750, solve(Source.fromResource("day17").mkString))

  @Test def t2(): Unit =
    val solve = Day17.parse andThen Day17.part2
    assertEquals(112, solve("target area: x=20..30, y=-10..-5"))
    assertEquals(4120, solve(Source.fromResource("day17").mkString))
