import aoc2021.Util.{Point, drawPoints, stringLines}

object Day13:
  enum Axis:
    case X(value: Int);
    case Y(value: Int);

  type Input = (Set[Point], Seq[Axis])

  def parse(input: String): Input =
    input.split("\n\n") match {
      case Array(pointsString, foldsString) => {
        val pts = stringLines(pointsString).map { line =>
          line.split(',') match {
            case Array(x, y) => (x.toInt, y.toInt)
            case _ => throw new Exception(f"Malformed input: ${line}")
          }
        }

        val FoldPattern = "fold along ([xy])=(\\d+)".r
        val folds = stringLines(foldsString).map { line =>
          line match {
            case FoldPattern("x", value) => Axis.X(value.toInt)
            case FoldPattern("y", value) => Axis.Y(value.toInt)
          }
        }

        (pts.toSet, folds)
      }
    }

  def foldPoints(pts: Set[Point], axis: Axis) =
    def foldAxis(ax: Int, v: Int): Int = if (v > ax) then 2 * ax - v else v

    pts.map { case (x, y) =>
      axis match {
        case Axis.X(ax) => (foldAxis(ax, x), y)
        case Axis.Y(ax) => (x, foldAxis(ax, y))
      }
    }

  def part1(input: Input) =
    val (pts, folds) = input
    foldPoints(pts, folds.head).size

  def part2(input: Input) =
    val (pts, folds) = input
    drawPoints(folds.foldLeft(pts)(foldPoints(_, _)).map((_, '#')).toMap)
