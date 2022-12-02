import aoc2021.Util.*

object Day21:
  type Input = Any
  //             Rolls  Score  Position
  type Player = (Int,   Int,   Int)
  type Game = (Player, Player)

  def parse(input: String): Game =
    val Pattern = "Player . starting position: (\\d+)".r
    stringLines(input) match {
      case Seq(Pattern(p1), Pattern(p2)) => ((1, 0, p1.toInt), (2, 0, p2.toInt))
      case _ => throw new Exception("Malformed input")
    }

  def board: Stream[Int] = (1 to 10).toStream #::: board

  def step(dice: Iterator[Int])(player: Player): Player =
    def move(rolls: Int, score: Int, position: Int) =
      val numbers = dice.take(3).toList
      val newPosition = board.drop(position + numbers.sum - 1).head
      println(s"${position} + ${numbers} -> ${newPosition} // ${score} -> ${score + newPosition}")
      (rolls + 3, score + newPosition, newPosition)
    move.tupled(player)

  def part1(game: Game) =
    def noWinner(p1: Player, p2: Player) =
      (p1, p2) match {
        case ((rolls1, score1, _), (rolls2, score2, _)) if score1 >= 1000 => Some((rolls1 + rolls2, score2))
        case ((rolls1, score1, _), (rolls2, score2, _)) if score2 >= 1000 => Some((rolls1 + rolls2, score1))
        case _ => None
      }

    def dice: Stream[Int] = (1 to 100).toStream #::: dice

    val s = step(dice.toIterator)
    val (p1, p2) = game
    val games = Stream.iterate(p1)(s).zip(Stream.iterate(p2)(s))
    val (p1x, p2x) = games.takeWhile(noWinner.tupled(_).isEmpty).takeRight(2).last
    (p1x, p2x) match {
      case ((rolls1, score1, _), (rolls2, score2, _)) if score2 < score1 => (rolls1 + rolls2) * score2
      case ((rolls1, score1, _), (rolls2, score2, _)) => (rolls1 + rolls2) * score1
    }

  def part2(game: Game) =
    game
