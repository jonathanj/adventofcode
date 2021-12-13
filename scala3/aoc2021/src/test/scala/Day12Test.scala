import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day12Test:
  val sample =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |""".stripMargin

  val sample2 =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin

  val sample3 =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin

  @Test def t1(): Unit =
    val solve = Day12.parse andThen Day12.part1
    assertEquals(10, solve(sample))
    assertEquals(19, solve(sample2))
    assertEquals(226, solve(sample3))
    assertEquals(3463, solve(Source.fromResource("day12").mkString))

  @Test def t2(): Unit =
    val solve = Day12.parse andThen Day12.part2
    assertEquals(36, solve(sample))
    assertEquals(103, solve(sample2))
    assertEquals(3509, solve(sample3))
    assertEquals(91533, solve(Source.fromResource("day12").mkString))
