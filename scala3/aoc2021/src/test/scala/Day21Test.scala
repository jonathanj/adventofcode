import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day21Test:
  val sample =
    """Player 1 starting position: 4
      |Player 2 starting position: 8
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day21.parse andThen Day21.part1
    assertEquals(739785, solve(sample))
    assertEquals(903630, solve(Source.fromResource("day21").mkString))

//  @Test def t2(): Unit =
//    val solve = Day21.parse andThen Day21.part2
//    assertEquals(3351, solve(sample))
//    assertEquals(18516, solve(Source.fromResource("day21").mkString))
