import aoc2021.Util.{Grid, Point, stringLines, gridFromLines, binaryToInt}

object Day20:
  type GridPixels = Grid[Boolean]
  type Input = (IndexedSeq[Boolean], (GridPixels, (Int, Int)))

  def parse(input: String): Input =
    input.split("\n\n") match {
      case Array(enhancementString, imageString) =>
        (enhancementString.map(_ == '#'), gridFromLines(stringLines(imageString))(_ == '#'))
    }

  def pixelIndex(pt: Point, grid: GridPixels, void: Boolean): Int =
    binaryToInt((for {
      y <- pt._2 - 1 to pt._2 + 1
      x <- pt._1 - 1 to pt._1 + 1
    } yield if grid.getOrElse((x, y), void) then '1' else '0').mkString)

  def enhance(count: Int)(input: Input): Int =
    val (enhancement, (grid, (w, h))) = input
    for (n <- 1 to count) {
      val tmpGrid = grid.clone()
      // Account for the infinity pixels (idx 0) alternating between on and off, when enhancement(0) is `true`.
      val void = if enhancement(0) then n % 2 == 0 else false
      for (y <- (0 - n) to (h + n)) {
        for (x <- (0 - n) to (w + n)) {
          val pt = (x, y)
          grid.put(pt, enhancement(pixelIndex(pt, tmpGrid, void)))
        }
      }
    }
    grid.values.count(identity)

  def part1 = enhance(2)

  def part2 = enhance(50)
