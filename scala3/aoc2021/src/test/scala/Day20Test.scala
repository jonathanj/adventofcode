import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day20Test:
  val sample =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day20.parse andThen Day20.part1
    assertEquals(35, solve(sample))
    assertEquals(5819, solve(Source.fromResource("day20").mkString))

  @Test def t2(): Unit =
    val solve = Day20.parse andThen Day20.part2
    assertEquals(3351, solve(sample))
    assertEquals(18516, solve(Source.fromResource("day20").mkString))
