package aoc2021

object Util:
  type Point = (Int, Int)
  
  def stringLines(input: String): Seq[String] =
    input.linesIterator.toSeq

  def stringLinesToLongs(input: String): Seq[Long] =
    stringLines(input).map(_.toLong)

  def frequencies[T](input: Seq[T]): Map[T, Long] =
    input.groupMapReduce(identity)(_ => 1L)(_ + _)

  def separatedInts(input: String): Seq[Int] =
    input.trim.split(",").map(_.toInt)

  def drawPoints(pts: Map[Point, Char]): String =
    val sb = new StringBuilder()
    val width = pts.keys.maxBy(_._1)._1
    val height = pts.keys.maxBy(_._2)._2
    for (y <- 0 to height) {
      for (x <- 0 to width) {
        sb ++= pts.getOrElse((x, y), '.').toString()
      }
      sb ++= "\n"
    }
    sb.toString()
