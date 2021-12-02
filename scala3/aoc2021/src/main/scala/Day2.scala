object Day2:
  enum Direction:
    case Forward(value: Long)
    case Up(value: Long)
    case Down(value: Long)

  type State = (Long, Long, Long)
  type ExecutorFunction = (State, Direction) => State

  def parse(input: String): Seq[Direction] =
    input.linesIterator.map { line =>
      line.split(' ') match {
        case Array(dir, cnt) => dir match {
          case "forward" => Direction.Forward(cnt.toLong)
          case "up" => Direction.Up(cnt.toLong)
          case "down" => Direction.Down(cnt.toLong)
        }
        case _ => throw Error("Malformed input")
      }
    }.toSeq

  def solve(input: Seq[Direction])(exec: ExecutorFunction) =
    val initial = (0L, 0L, 0L)
    val (x, y, _) = input.foldLeft(initial)(exec)
    x * y

  def part1(input: Seq[Direction]) =
    solve(input) { case ((x, y, _), dir) =>
      dir match {
        case Direction.Forward(cnt) => (x + cnt, y, 0)
        case Direction.Up(cnt) => (x, y - cnt, 0)
        case Direction.Down(cnt) => (x, y + cnt, 0)
      }
    }

  def part2(input: Seq[Direction]) =
    solve(input) { case ((x, y, aim), dir) =>
      dir match {
        case Direction.Forward(cnt) => (x + cnt, y + (aim * cnt), aim)
        case Direction.Up(cnt) => (x, y, aim - cnt)
        case Direction.Down(cnt) => (x, y, aim + cnt)
      }
    }
