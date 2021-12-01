package aoc2021

object Util:
  def stringLinesToLongs(input: String): Seq[Long] =
    input.linesIterator.map(_.toLong).toSeq
