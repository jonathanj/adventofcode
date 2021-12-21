import aoc2021.Util.{Grid, Point, gridFromLines, stringLines, dijkstra, tracePath, divmod}

object Day15:
  type GridRisk = Grid[Int]
  type Input = (GridRisk, (Int, Int))

  def parse(input: String): Input = gridFromLines(stringLines(input))(_ - '0')

  def adj(limits: (Int, Int), weight: (Point, Point) => Int)(a: Point) =
    val (x, y) = a
    val (w, h) = limits
    for {
      b @ (x, y) <- List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      if x >= 0 && y >= 0 && x <= w && y <= h
    } yield (b, weight(a, b))

  def solve(input: Input)(lookup: Point => Int, end: Point) =
    def cost(a: Point, b: Point) = lookup(a) + lookup(b)
    val prev = dijkstra(adj(end, cost))((0, 0))._2
    tracePath(prev, end).map(lookup).sum

  def part1(input: Input) =
    val (grid, (width, height)) = input
    val end = (width - 1, height - 1)
    solve(input)(grid, end)

  def part2(input: Input) =
    val (grid, (width, height)) = input
    // Adjust the original value of a coordinate by the below matrix:
    // 0 1 2 3 4
    // 1 2 3 4 5
    // 2 3 4 5 6
    // 3 4 5 6 7
    // 4 5 6 7 8
    val adjustment = (0 until 5).map { n => (0 to 8).slice(n, n + 5).toArray }.toArray

    def lookup5(pt: Point) =
      val (x, y) = pt
      val (ix, iix) = divmod(x, width)
      val (iy, iiy) = divmod(y, height)
      val iv = grid((iix, iiy))
      (ix, iy, iv) match {
        case (0, 0, iv) => iv
        case (ix, iy, iv) =>
          val tmp = iv + adjustment(iy)(ix)
          if tmp > 9 then tmp % 10 + 1 else tmp
      }

    val end = (width * 5 - 1, height * 5 - 1)
    solve(input)(lookup5, end)
