import aoc2021.Util

object Day9:
  type Point = (Int, Int)
  case class World[T](width: Int, height: Int) extends scala.collection.mutable.HashMap[Point, T]
  type Input = World[Int]

  def parse(input: String): Input =
    val lines = Util.stringLines(input)
    val height = lines.length
    val width = lines(0).length
    val world = World[Int](width, height)
    for ((line, y) <- lines.zipWithIndex) {
      for ((ch, x) <- line.zipWithIndex) {
        world.put((x, y), ch - '0')
      }
    }
    world

  def neighbours(world: Input)(pt: Point) =
    val (x, y) = pt
    List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      .filter { case (x, y) => x >= 0 && x < world.width && y >= 0 && y < world.height }
      .toSet

  def lowpoints(world: Input) =
    world.filter { case ((pt, height)) => neighbours(world)(pt).forall(world(_) > height) }.keys.toSeq

  def part1(world: Input) =
    lowpoints(world).map(world(_) + 1).sum

  def part2(world: Input) =
    def basin(queue: Seq[Point], seen: Set[Point] = Set()): Set[Point] =
      queue match {
        case Seq() => seen
        case pt :: rest => {
          val ns = neighbours(world)(pt).diff(seen).filter(world(_) < 9)
          basin(ns.toSeq ++ rest, seen | Set(pt) | ns)
        }
      }

    lowpoints(world).map { pt => basin(Seq(pt)).size.toLong }.sorted.takeRight(3).product
