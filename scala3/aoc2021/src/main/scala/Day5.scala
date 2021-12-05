import aoc2021.Util

object Day5:
  type Coordinate = (Int, Int)
  type Input = Seq[(Coordinate, Coordinate)]

  def parse(input: String): Input =
    Util.stringLines(input).map { line =>
      line.split(" -> ").flatMap(_.split(',')).map(_.toInt) match {
        case Array(x1, y1, x2, y2) => ((x1, y1), (x2, y2))
        case _ => throw new Exception("Malformed input")
      }
    }

  type PointsPredicate = ((Int, Int)) => Boolean

  /**
   * Generate all the intermediate coordinates between `a` and `b`.
   */
  def points(pred: PointsPredicate)(a: Coordinate, b: Coordinate): Seq[Coordinate] =
    val (x1, y1) = a
    val (x2, y2) = b
    val xstep = (x2 - x1).sign
    val ystep = (y2 - y1).sign

    if pred((xstep, ystep)) then
      val (xs, ys) = if xstep == 0 then
        (Stream.continually(x1), y1 to y2 by ystep)
      else if ystep == 0 then
        (x1 to x2 by xstep, Stream.continually(y1))
      else
        (x1 to x2 by xstep, y1 to y2 by ystep)
      xs zip ys
    else
      Seq()

  type PointsFunction = (Coordinate, Coordinate) => Seq[Coordinate]

  def solve(pointsFn: PointsFunction)(input: Input): Int =
    val plane = Map[Coordinate, Int]()
    input.flatMap(pointsFn.tupled).foldLeft(plane) { case (plane, coord) =>
      plane.updatedWith(coord)(_.orElse(Some(0)).map(_ + 1))
    }.values.filter(_ > 1).size

  val noDiagonals: PointsPredicate = { case (xstep, ystep) => xstep == 0 || ystep == 0 }
  def part1 = solve(points(noDiagonals))

  def part2 = solve(points(Function.const(true)))
