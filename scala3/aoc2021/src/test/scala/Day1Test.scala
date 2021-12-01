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
    assertEquals(7, Day1.part1(Day1.parse(sample)))
    assertEquals(1709, Day1.part1(Day1.parse(Source.fromResource("day1").mkString)))

  @Test def t2(): Unit =
    assertEquals(5, Day1.part2(Day1.parse(sample)))
    assertEquals(1761, Day1.part2(Day1.parse(Source.fromResource("day1").mkString)))
