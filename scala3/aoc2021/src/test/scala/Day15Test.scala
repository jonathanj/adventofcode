import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day15Test:
  val sample =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day15.parse andThen Day15.part1
    assertEquals(40, solve(sample))
    assertEquals(403, solve(Source.fromResource("day15").mkString))

  @Test def t2(): Unit =
    val solve = Day15.parse andThen Day15.part2
    assertEquals(315, solve(sample))
    assertEquals(2840, solve(Source.fromResource("day15").mkString))
