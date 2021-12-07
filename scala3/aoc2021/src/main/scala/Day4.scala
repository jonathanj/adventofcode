import aoc2021.Util

object Day4:
  type Board = List[List[Int]]
  type Input = (List[Int], Seq[Board], Set[Int])

  def parse(input: String): Input =
    Util.stringLines(input).filter(_.nonEmpty) match {
      case draw :: rest =>
        val numbers = draw.split(',').map(_.toInt).toList
        val boards = rest
          // Split rows into numbers
          .map(_.trim.split("\\s+").map(_.toInt).toList)
          // Group rows into boards
          .grouped(5).toSeq
        (numbers, boards, Set[Int]())
    }

  // Mark called numbers as -1.
  def mark(number: Int)(board: Board) = board.map(_.map { x => if x == number then -1 else x })
  def score(number: Int)(board: Board) = number * board.flatten.filter(_ != -1).sum
  def isWinner(board: Board) = board.exists(_.forall(_ == -1)) || board.transpose.exists(_.forall(_ == -1))

  /**
   * Find the winning scores for every board, in the order they are won.
   */
  def play(numbers: List[Int], boards: Seq[Board], marked: Set[Int] = Set()): Seq[Int] =
    numbers match {
      case Seq() => Seq()
      case number :: numbers => {
        val (winners, losers) = boards.map(mark(number)).partition(isWinner)
        winners.map(score(number)) ++ play(numbers, losers, marked + number)
      }
    }

  def part1 = play.tupled andThen (_.head)

  def part2 = play.tupled andThen (_.last)
