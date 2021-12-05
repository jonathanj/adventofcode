import aoc2021.Util

object Day4:
  type Board = List[List[Int]]
  type Input = (Seq[Int], Set[Board])

  def parse(input: String): Input =
    Util.stringLines(input).filter(_.nonEmpty) match {
      case draw :: rest =>
        val numbers = draw.split(',').map(_.toInt).toSeq
        val boards = rest
          // Split rows into numbers
          .map(_.trim.split("\\s+").map(_.toInt).toList)
          // Group rows into boards
          .grouped(5).toSet
        (numbers, boards)
    }

  /**
   * Find the winning scores for every board, in the order they are won.
   */
  def winningScores(input: Input) =
    val (allNumbers, allBoards) = input
    // Generate regular and transposed versions of every board, this is easier to check for winners.
    val boards = allBoards.map { board => board ++ board.transpose }
    def winners(numbers: Seq[Int]) = boards.filter(_.exists(_.toSet.subsetOf(numbers.toSet)))

    def score(board: Board, marked: Seq[Int]) =
      board.flatten.toSet.diff(marked.toSet).sum * marked.last

    allNumbers
      // Build the numbers for every round, e.g. [[7], [7,4], [7,4,9], …]
      .scanLeft(Seq[Int]())(_ :+ _)
      // Find the scores for every winner for every round.
      .scanLeft((Set[Int](), Set[Board]())) { case ((_, previousWinners), numbers) =>
        val newWinners = winners(numbers)
        (newWinners.diff(previousWinners).map(score(_, numbers)), newWinners)
      }
      // Throw away results with no winners.
      .filter(!_._1.isEmpty)
      // Assume that the answers we care about only have a single result.
      .map(_._1.head)

  def part1 = winningScores andThen (_.head)

  def part2 = winningScores andThen (_.last)
