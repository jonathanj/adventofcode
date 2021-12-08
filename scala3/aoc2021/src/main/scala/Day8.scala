import aoc2021.Util

object Day8:
  type SignalsAndDigits = (Seq[String], Seq[String])
  type Input = Seq[SignalsAndDigits]

  def parse(input: String): Input =
    Util.stringLines(input).map { line =>
      line.split(" \\| ") match {
        case Array(signals, digits) => (signals.split(' ').toSeq, digits.split(' ').toSeq)
      }
    }

  def part1(input: Input) =
    input.map { case (_, digits) =>
      digits.map(_.size).filter(Set(2, 4, 3, 7).contains(_))
    }.flatten.size

  /**
   * By process of eliminating ambiguous segments from digits, using known
   * configurations of other digits, it's possible to deduce all ten digits.
   */
  def solveByElimination(pair: SignalsAndDigits) =
    val (signals, digits) = pair
    val bySegmentCount = signals.map(_.toSet).groupBy(_.size).mapValues(_.toSet).toMap
    val one = bySegmentCount(2).head
    val four = bySegmentCount(4).head
    val seven = bySegmentCount(3).head
    val eight = bySegmentCount(7).head

    val three = bySegmentCount(5).find(_.diff(seven).size == 2).get
    val five = (bySegmentCount(5) - three).find(_.diff(three | four).size == 0).get
    val two = (bySegmentCount(5) - three - five).head

    val nine = bySegmentCount(6).find(_.diff(four).size == 2).get
    val zero = (bySegmentCount(6) - nine).find(_.diff(five).size == 2).get
    val six = (bySegmentCount(6) - zero - nine).head

    val m = Map(zero -> '0', one -> '1', two -> '2', three -> '3', four -> '4',
                five -> '5', six -> '6', seven -> '7', eight -> '8', nine -> '9')
    digits.map { digits => m(digits.toSet) }.mkString.toInt


  /**
   * Create a "rainbow table" for every possible wiring to every other possible
   * wiring, find the wiring that decodes the signals into known keys of `segments`,
   * and decode the digits with that wiring.
   */
  val segments = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg").zipWithIndex.toMap
  val wires = 'a' to 'g'
  val allRewirings = wires.permutations.map(wires.zip(_).toMap).toSeq
  def solveByBruteForce(pair: SignalsAndDigits) =
    val (signals, digits) = pair
    val wiring = allRewirings.find { wiring => signals.map(_.map(wiring).sorted).toSet.subsetOf(segments.keySet) }.head
    digits.map(_.map(wiring).sorted).map(segments).mkString.toInt

  def part2(input: Input) =
    //input.map(solveByElimination).sum
    input.map(solveByBruteForce).sum
