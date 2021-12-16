import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day14Test:
  val sample =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day14.parse andThen Day14.part1
    assertEquals(1588, solve(sample))
    assertEquals(2891, solve(Source.fromResource("day14").mkString))

  @Test def t2(): Unit =
    val solve = Day14.parse andThen Day14.part2
    assertEquals(2188189693529L, solve(sample))
    assertEquals(4607749009683L, solve(Source.fromResource("day14").mkString))
