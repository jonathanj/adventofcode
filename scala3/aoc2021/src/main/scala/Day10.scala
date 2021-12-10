import aoc2021.Util

object Day10:
  type Input = Seq[String]

  def parse = Util.stringLines

  val closing = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def corruption(s: String) =
    def recur(chars: Seq[Char], stack: List[Char]): (Option[Char], List[Char]) =
      chars match {
        case Seq() => (None, stack)
        case ch :: chars => (ch, stack.headOption) match {
          case   ('(', _)
               | ('[', _)
               | ('{', _)
               | ('<', _) => recur(chars, closing(ch) :: stack)
          case (ch, Some(mch)) if ch == mch => recur(chars, stack.tail)
          case _ => (Some(ch), stack)
        }
      }
    recur(s.toList, List())

  def errorScore(chars: Seq[Char]) =
    val scoring = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    Util.frequencies(chars).map { (ch, n) => n * scoring(ch) }.sum

  def part1(input: Input) =
    errorScore(input.flatMap(corruption andThen (_._1)))

  def autocompleteScore(chars: Seq[Char]) =
    val scoring = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    chars.foldLeft(0L) { (score, ch) => score * 5 + scoring(ch) }

  def part2(input: Input) =
    val scores = input.map(corruption).collect { case (None, stack) => autocompleteScore(stack) }.sorted
    scores(scores.length / 2)
