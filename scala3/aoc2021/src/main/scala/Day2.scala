object Day2:
  type State = (Long, Long, Long)
  type Instruction = (String, Long)
  type ExecutorFunction = (State, Instruction) => State

  def parse(input: String): Seq[Instruction] =
    input.linesIterator.map { line =>
      line.split(' ') match {
        case Array(dir, cnt) => (dir, cnt.toLong)
        case _ => throw Error("Malformed input")
      }
    }.toSeq

  def solve(input: Seq[Instruction])(exec: ExecutorFunction) =
    val initial = (0L, 0L, 0L)
    val (x, y, _) = input.foldLeft(initial) { case (state, instruction) =>
      exec(state, instruction)
    }
    x * y

  def part1(input: Seq[Instruction]) =
    solve(input) { case ((x, y, _), (dir, cnt)) =>
      dir match {
        case "forward" => (x + cnt, y, 0)
        case "up" => (x, y - cnt, 0)
        case "down" => (x, y + cnt, 0)
      }
    }

  def part2(input: Seq[Instruction]) =
    solve(input) { case ((x, y, aim), (dir, cnt)) =>
      dir match {
        case "forward" => (x + cnt, y + (aim * cnt), aim)
        case "up" => (x, y, aim - cnt)
        case "down" => (x, y, aim + cnt)
      }
    }
