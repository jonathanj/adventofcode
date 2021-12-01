import scala.io.Source
import org.junit.Test
import org.junit.Assert.*

class Day1Test:
  val sample =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day1.parse andThen Day1.part1
    assertEquals(7, solve(sample))
    assertEquals(1709, solve(Source.fromResource("day1").mkString))

  @Test def t2(): Unit =
    val solve = Day1.parse andThen Day1.part2
    assertEquals(5, solve(sample))
    assertEquals(1761, solve(Source.fromResource("day1").mkString))
