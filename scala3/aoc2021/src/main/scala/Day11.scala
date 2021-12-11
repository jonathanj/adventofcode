import aoc2021.Util

object Day11:
  type Point = (Int, Int)
  type Input = scala.collection.mutable.HashMap[Point, Int]

  def parse(input: String): Input =
    val lines = Util.stringLines(input)
    val height = lines.length
    val width = lines(0).length
    val world = scala.collection.mutable.HashMap[Point, Int]()
    for ((line, y) <- lines.zipWithIndex) {
      for ((ch, x) <- line.zipWithIndex) {
        world.put((x, y), ch - '0')
      }
    }
    world

  def neighbours(world: Input)(pt: Point): Set[Point] =
    val (x, y) = pt
    List(
      (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
      (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1))
      .filter { case pt => world.contains(pt) }
      .toSet

  def step(world: Input) =
    def flash(queue: Seq[Point], seen: Set[Point]): Set[Point] =
      seen.foreach { pt => world.put(pt, 0) }

      queue match {
        case Seq() => seen
        case pt :: rest => {
          val ns = neighbours(world)(pt).diff(seen)
          ns.foreach { pt => world.put(pt, world(pt) + 1) }
          val flashedNeighbours = ns.collect { case pt if world(pt) > 9 => pt }
          val newSeen = seen | flashedNeighbours
          flash(flashedNeighbours.toSeq ++ rest, newSeen)
        }
      }

    world.foreach { (pt, v) => world.put(pt, v + 1) }
    val seen = world.filter { (_, v) => v > 9 }.keySet.toSet
    flash(seen.toSeq, seen)

  def part1(world: Input) =
    var total = 0
    for (n <- 1 to 100) {
      total = total + step(world).size
    }
    total

  def part2(world: Input): Int =
    var n = 0
    while (true) {
      n = n + 1
      val flashed = step(world)
      if (flashed.size == world.size) {
        return n
      }
    }
    throw new Exception("Hello")
