import aoc2021.Util.*

object Day22:
  case class Cuboid(x1: Long, x2: Long, y1: Long, y2: Long, z1: Long, z2: Long):
    val isInitProcedure: Boolean = x1 >= -50 && x2 <= 50 && y1 >= -50 && y2 <= 50 && z1 >= -50 && z2 <= 50
    val volume: Long = (x2 - x1 + 1L) * (y2 - y1 + 1L) * (z2 - z1 + 1L)

    def intersect(other: Cuboid): Option[Cuboid] =
      val (nx1, nx2) = (Math.max(x1, other.x1), Math.min(x2, other.x2))
      val (ny1, ny2) = (Math.max(y1, other.y1), Math.min(y2, other.y2))
      val (nz1, nz2) = (Math.max(z1, other.z1), Math.min(z2, other.z2))
      if nx1 <= nx2 && ny1 <= ny2 && nz1 <= nz2 then
        Some(Cuboid(nx1, nx2, ny1, ny2, nz1, nz2))
      else
        None

  type Instruction = (Boolean, Cuboid)
  type Input = Seq[Instruction]

  def parse(input: String): Input =
    val Pattern = "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r
    stringLines(input).map {
      case Pattern(state, x1, x2, y1, y2, z1, z2) => (state == "on", Cuboid(x1.toLong, x2.toLong, y1.toLong, y2.toLong, z1.toLong, z2.toLong))
      case _ => throw new Exception("Malformed input")
    }

  // https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle
  // Add up all the cuboid volumes, which will double count the overlaps, but
  // then subtract the volumes for the overlapping parts.
  def solve(cuboids: List[Instruction]): Long =
    cuboids match {
      case List() => 0
      case (state, cuboid) :: rest if !state => solve(rest)
      case (state, cuboid) :: rest =>
        cuboid.volume + solve(rest) - solve(rest.flatMap { case (_, c) => cuboid.intersect(c).map((true, _)) }.distinct)
    }

  def part1(input: Input) =
    solve(input.filter(_._2.isInitProcedure).toList)

  def part2(input: Input) =
    solve(input.toList)
