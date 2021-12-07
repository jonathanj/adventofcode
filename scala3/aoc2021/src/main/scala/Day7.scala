import aoc2021.Util

object Day7:
  def parse = Util.separatedInts

  type CostFunction = (Int, Int) => Int

  def fuelcost(cost: CostFunction)(input: Seq[Int]) =
    (input.min to input.max).map { target => input.map { crab => cost(crab, target) }.sum }.min

  def diff(crab: Int, target: Int) = (crab - target).abs
  def part1 = fuelcost(diff)

  def compound(crab: Int, target: Int) = (1 to diff(crab, target)).sum
  def part2 = fuelcost(compound)
