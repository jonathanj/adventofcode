import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day25Test:
  val sample =
    """v...>>.vv>
      |.vv>>.vv..
      |>>.>v>...v
      |>>v>>.>.v.
      |v>v.vv.v..
      |>.>>..v...
      |.vv..>.>v.
      |v.v..>>v.v
      |....v..v.>
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day25.parse andThen Day25.part1
    assertEquals(58, solve(sample))
    assertEquals(435, solve(Source.fromResource("day25").mkString))
