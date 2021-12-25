import aoc2021.Util.*

object Day25:
  type Direction = (Int, Int)
  val South: Direction = (0, 1)
  val East: Direction = (1, 0)
  type Grid = (Map[Point, Direction], (Int, Int))

  def parse(input: String): Grid =
    val (grid, wh) = gridFromLines(stringLines(input)) {
      case 'v' => South
      case '>' => East
    }
    (grid.toMap, wh)

  def advance(gridwh: Grid, pt: Point): Option[(Point, Point)] =
    val (grid, (w, h)) = gridwh
    val (dx, dy) = grid(pt)
    val npt = ((pt._1 + dx) % w, (pt._2 + dy) % h)
    if !grid.contains(npt) then Some((pt, npt)) else None

  def step(gridwh: Grid): Grid =
    def move(candidates: Iterable[Point])(gridwh: Grid): Grid =
      val pts = candidates.flatMap(advance(gridwh, _))
      (pts.foldLeft(gridwh._1) { case (grid, (pt, npt)) => (grid - pt) + (npt -> grid(pt)) }, gridwh._2)

    val (east, south) = gridwh._1.partition(_._2 == East)
    (move(east.keys) andThen move(south.keys))(gridwh)

  def part1(gridwh: Grid) =
    val steps = LazyList.iterate(gridwh)(step)
    steps.zip(steps.tail).indexWhere { case (a, b) => a == b } + 1
