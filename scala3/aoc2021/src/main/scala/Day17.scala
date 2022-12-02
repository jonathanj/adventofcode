import aoc2021.Util.Point

object Day17:
  type Target = (Int, Int, Int, Int)

  def parse(input: String): Target =
    val Pattern = "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
    input.trim match {
      case Pattern(x1, x2, y1, y2) => (x1.toInt, x2.toInt, y1.toInt, y2.toInt)
      case _ => throw new Exception("Malformed input")
    }

  def simulate(initialVelocity: (Int, Int)) =
    // FIXME: This needs to be based on the initial trajectory
    val dx = -1
    val dy = -1

    def sim(state: ((Int, Int), Point)) =
      val (velocity, current) = state
      (velocity, current) match {
        case ((0, vy), (x, y)) => ((0, vy + dy), (x, y + vy))
        case ((vx, vy), (x, y)) => ((vx + dx, vy + dy), (x + vx, y + vy))
      }

    Stream.iterate((initialVelocity, (0, 0)))(sim).take(1000)

  def isInsideTarget(target: Target)(pt: Point) =
    val (x1, x2, y1, y2) = target
    val (px, py) = pt
    px >= x1 && px <= x2 && py >= y1 && py <= y2

  def hasMissed(target: Target)(pt: Point) =
    val (x1, x2, y1, y2) = target
    val (px, py) = pt
    println(f"hasMissed: target(${target}), pt(${pt})")
    if x1 > x2 then
      px > x2 || py < y2
    else
      px > x1 || py < y2

  def part1(target: Target) =
    def one(traj: (Int, Int)) =
      val points = simulate(traj)
      if points.exists { case (_, pt) => isInsideTarget(target)(pt) } then
        Some(points.map(_._2._2).max)
      else
        None

    val (x1, x2, y1, y2) = target
    val sx = if x1 < 0 then -1 else 1
    val sy = 1
    if y1 > 0 then throw new Exception("What the heck")

    def sample() =
      (for {
        tx <- 1 to (x2 / 2) by sx
        ty <- 1 to 2000
      } yield one((tx, ty))).flatten

    sample().max

  def part2(target: Target) =
    def one(traj: (Int, Int)) =
      val points = simulate(traj)
      if points.exists { case (_, pt) => isInsideTarget(target)(pt) } then
        Some(points.map(_._2._2).max)
      else
        None

    val (x1, x2, y1, y2) = target
    val sx = if x1 < 0 then -1 else 1
    val sy = 1
    if y1 > 0 then throw new Exception("What the heck")

    def sample() =
      for {
        tx <- 1 to x2 by sx
        ty <- y1 to y1 * -1
        if one((tx, ty)).isDefined
      } yield (tx, ty)

    sample().toSet.size
