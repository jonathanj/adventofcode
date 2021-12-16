import aoc2021.Util.{stringLines, frequencies}

object Day14:
  type Pair = (Char, Char)
  type Rules = Map[Pair, Char]
  type Input = (String, Rules)

  def parse(input: String): Input =
    input.split("\n\n") match {
      case Array(initial, rulesString) => {
        val Pattern = "(.)(.) -> (.)".r
        val rules = stringLines(rulesString).map { s => s match {
          case Pattern(a, b, c) => ((a(0), b(0)), c(0))
          case _ => throw new Exception(f"Input malformed: ${s}")
        } }.toMap
        (initial, rules)
      }
    }

  def updatedMapKey[A, B](m: Map[A, B], k: A, default: B)(f: B => B): Map[A, B] =
    m.updatedWith(k) { ov => ov.orElse(Some(default)).map(f) }

  def incKey(m: Map[Pair, Long], k: Pair)(n: Long) = updatedMapKey(m, k, 0L)(_ + n)

  def solve(input: Input) =
    val (seed, rules) = input
    val initial = Map[Pair, Long]()
    val pairs = seed.zip(seed.tail).foldLeft(initial) { (pairs, ab) => updatedMapKey(pairs, ab, 0L)(_ + 1L) }
    Stream.iterate(pairs) { pairs =>
      pairs.foldLeft(pairs) { case (pairs, ((a, b), count)) =>
        val c = rules((a, b))
        List[(Pair, (Long) => Long)] (
          ((a, b), (_ - count)),
          ((a, c), (_ + count)),
          ((c, b), (_ + count))
        ).foldLeft(pairs) { case (pairs, (key, f)) => updatedMapKey(pairs, key, 0L)(f) }
      }
    }

  def count(seed: String, m: Map[Pair, Long]) =
    val res = m.foldLeft(Map[Char, Long]()) { case (res, ((a, _), count)) =>
      updatedMapKey(res, a, 0L)(_ + count)
    }.updatedWith(seed.last) { ov => ov.orElse(Some(0L)).map(_ + 1) }.values
    res.max - res.min

  def part1(input: Input) =
    count(input._1, solve(input)(10))

  def part2(input: Input) =
    count(input._1, solve(input)(40))
