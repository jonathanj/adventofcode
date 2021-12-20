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

  type Grid[T] = scala.collection.mutable.HashMap[Point, T]

  def gridFromLines[T](lines: Seq[String])(transform: Char => T): (Grid[T], (Int, Int)) =
    val grid = scala.collection.mutable.HashMap[Point, T]()
    for ((line, y) <- lines.zipWithIndex) {
      for ((ch, x) <- line.zipWithIndex) {
        grid.put((x, y), transform(ch))
      }
    }
    (grid, (lines.head.length, lines.length))

  def drawPoints(pts: Map[Point, Char]): String =
    val sb = new StringBuilder()
    val ox = pts.keys.minBy(_._1)._1
    val oy = pts.keys.minBy(_._2)._2
    val ow = pts.keys.maxBy(_._1)._1
    val oh = pts.keys.maxBy(_._2)._2
    for (y <- oy to oh) {
      for (x <- ox to ow) {
        sb ++= pts.getOrElse((x, y), '.').toString()
      }
      sb ++= "\n"
    }
    sb.toString()

  val hexCharToBinary = Map(
    '0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011", '4' -> "0100", '5' -> "0101", '6' -> "0110",
    '7' -> "0111", '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011", 'C' -> "1100", 'D' -> "1101",
    'E' -> "1110", 'F' -> "1111")

  def hexToBinary(hex: String): String = hex.trim.map(hexCharToBinary).mkString

  def binaryToInt(bin: String): Int = Integer.parseInt(bin, 2)
