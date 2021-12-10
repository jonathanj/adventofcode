import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day10Test:
  val sample =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |""".stripMargin

  @Test def t1(): Unit =
    val solve = Day10.parse andThen Day10.part1
    assertEquals(26397, solve(sample))
    assertEquals(290691, solve(Source.fromResource("day10").mkString))

  @Test def t2(): Unit =
    val solve = Day10.parse andThen Day10.part2
    assertEquals(288957, solve(sample))
    assertEquals(2768166558L, solve(Source.fromResource("day10").mkString))
