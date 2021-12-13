import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day13Test:
  val sample =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day13.parse andThen Day13.part1
    assertEquals(17, solve(sample))
    assertEquals(837, solve(Source.fromResource("day13").mkString))

  @Test def t2(): Unit =
    val solve = Day13.parse andThen Day13.part2
    assertEquals(
      """#####
        |#...#
        |#...#
        |#...#
        |#####
        |""".stripMargin, solve(sample))
    assertEquals(
      """####.###..####..##..#..#..##..#..#.#..#
        |#....#..#....#.#..#.#.#..#..#.#..#.#..#
        |###..#..#...#..#....##...#....####.#..#
        |#....###...#...#.##.#.#..#....#..#.#..#
        |#....#....#....#..#.#.#..#..#.#..#.#..#
        |####.#....####..###.#..#..##..#..#..##.
        |""".stripMargin, solve(Source.fromResource("day13").mkString))
