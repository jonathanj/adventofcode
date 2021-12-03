import aoc2021.Util

object Day3:
  type Input = Seq[String]
  type Frequencies = Seq[Map[Char, Long]]
  type FrequencyCriteria = (Map[Char, Long]) => Char

  def parse(input: String): Input =
    Util.stringLines(input)

  def frequencies(input: Input) =
    input.map(_.toList).transpose.map(Util.frequencies)

  val mostBits: FrequencyCriteria = _.maxBy(_._2)._1
  val fewestBits: FrequencyCriteria = _.minBy(_._2)._1

  private def simple(criteria: FrequencyCriteria)(freqs: Frequencies) =
    Integer.parseInt(freqs.map(criteria).mkString, 2)

  def gamma = simple(mostBits)
  def epsilon = simple(fewestBits)

  def part1(input: Input) =
    val freqs = frequencies(input)
    gamma(freqs) * epsilon(freqs)

  private def complex(criteria: FrequencyCriteria, fallback: Char) =
    def fn(input: Input, idx: Int): Int =
      input match {
        case Seq(result) => Integer.parseInt(result, 2)
        case input => {
          val freq = frequencies(input)(idx)
          val matchBit = if freq('1') == freq('0') then fallback else criteria(freq)
          fn(input.filter(_(idx) == matchBit), idx + 1)
        }
      }
    fn(_, 0)

  def o2 = complex(mostBits, '1')
  def co2scrubber = complex(fewestBits, '0')

  def part2(input: Input) =
    o2(input) * co2scrubber(input)
