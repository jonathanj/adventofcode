import aoc2021.Util.*

import scala.annotation.tailrec

object Day19:
  type Point3 = (Int, Int, Int)
  type Scanners = Seq[Seq[Point3]]

  def manhattanDist(a: Point3, b: Point3): Int =
    (a._1 - b._1).abs + (a._2 - b._2).abs + (a._3 - b._3).abs

  def add(a: Point3, b: Point3): Point3 =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)

  def diff(a: Point3, b: Point3): Point3 =
    (a._1 - b._1, a._2 - b._2, a._3 - b._3)

  def parse(input: String): Scanners =
    input.split("\n\n").map { s =>
      stringLines(s).tail.map {
        t => separatedInts(t) match {
          case Seq(a, b, c) => (a, b, c)
          case _ => throw new Exception(s"Malformed input: ${t}")
        }
      }
    }.toSeq

  def faces(pt: Point3): List[Point3] =
    val (x, y, z) = pt
    List(
      ( x,  y,  z),
      ( y, -x,  z),
      (-x, -y,  z),
      (-y,  x,  z),
      ( y,  z,  x),
      ( y, -z, -x))

  def rotations(pt: Point3): List[Point3] =
    val (x, y, z) = pt
    List(
      (x,  y,  z),
      (x, -z,  y),
      (x, -y, -z),
      (x,  z, -y))

  def orientations(pts: Seq[Point3]): Seq[Set[Point3]] =
    pts.map(rotations(_).flatMap(faces)).transpose.map(_.toSet)

  def viableOffsets(xs: Seq[Point3], ys: Seq[Point3]): Seq[Point3] =
    val pts = for {
      x <- xs
      y <- ys
    } yield diff(x, y)
    // Only consider offsets that occur at least 12 times, other offsets won't
    // contribute anything towards the result.
    frequencies(pts).filter((_, cnt) => cnt >= 12).keys.toSeq

  /**
   * Find some offset and orientation of `beacons` for which enough points
   * correspond to `reference`.
   */
  def findOrientation(reference: Set[Point3], beacons: Seq[Point3]): Option[(Point3, Set[Point3])] =
    (for {
      beaconSet <- orientations(beacons)
      // Keep the offset since it is the location of the scanner relative
      // to the reference.
      offset <- viableOffsets(reference.toSeq, beaconSet.toSeq)
      // Project the reoriented beacon positions by the offset.
      absoluteBeaconSet = beaconSet.map(add(offset, _))
      if reference.intersect(absoluteBeaconSet).size >= 12
    } yield (offset, absoluteBeaconSet)).headOption

  /**
   * Discover subsequent scanner offsets and set of beacons for each.
   */
  def discover(scanners: Scanners, results: List[(Point3, Set[Point3])]): List[(Point3, Set[Point3])] =
    results match {
      case List() => List()
      case (p @ (offset, ref)) :: ps =>
        // All scanners with no correlation to the reference points are
        // collected into `restScanners`, while the ones with correlations
        // become new results.
        val (newResults, restScanners) = (for {
          scanner <- scanners
        } yield findOrientation(ref, scanner) match {
          case None => Right(scanner)
          case Some(value) => Left(value)
        }).toList.partitionMap(identity)
        p :: discover(restScanners, newResults ::: ps)
    }

  def solve(scanners: Scanners) =
    // Make the assumption that the first scanner is at the origin and is in
    // the correct orientation, then find scanner and orientations from there.
    discover(scanners.tail, List(((0, 0, 0), scanners.head.toSet))).unzip

  def part1(scanners: Scanners) =
    val (_, locations) = solve(scanners)
    locations.flatten.toSet.size

  def part2(scanners: Scanners) =
    val (offsets, _) = solve(scanners)
    (for {
      a <- offsets
      b <- offsets
    } yield manhattanDist(a, b)).max
