package aoc2021

object Util:
  def stringLines(input: String): Seq[String] =
    input.linesIterator.toSeq

  def stringLinesToLongs(input: String): Seq[Long] =
    stringLines(input).map(_.toLong)

  def frequencies[T](input: Seq[T]): Map[T, Long] =
    input.groupMapReduce(identity)(_ => 1L)(_ + _)
