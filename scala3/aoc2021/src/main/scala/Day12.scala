import scala.collection.immutable.Queue
import aoc2021.Util

object Day12:
  type Input = Map[String, Set[String]]

  def parse(input: String): Input =
    val initial = Map[String, Set[String]]()
    Util.stringLines(input).foldLeft(initial) { (result, line) => line.split('-') match {
      case Array("start", b) =>
        result + ("start" -> result.getOrElse("start", Set[String]()).incl(b))
      case Array(a, b) =>
        result + (
          a -> result.getOrElse(a, Set[String]()).incl(b),
          b -> result.getOrElse(b, Set[String]()).incl(a))
    } } + ("end" -> Set())

  type VisitMaxFunction = (Map[String, Long]) => Long

  def traverse(tree: Input)(visitMax: VisitMaxFunction) =
    def bfs(start: String, end: String): List[List[String]] =
      var results = List[List[String]]()
      var queue = scala.collection.mutable.Queue[List[String]](List(start))
      var visited = scala.collection.mutable.Set[List[String]]()

      while (queue.nonEmpty) {
        val path = queue.dequeue
        val node = path.head
        if (node == end) {
          results = path :: results
        } else if (!visited.contains(path)) {
          val validNeighbours = tree.getOrElse(node, Set()).diff(path.filter { node =>
            node match {
              case "start" => true
              case node if node == node.toLowerCase => {
                val freqs = Util.frequencies(path.filter { s => s != "start" && s == s.toLowerCase })
                freqs.getOrElse(node, 0L) >= visitMax(freqs)
              }
              case _ => false
            }
          }.toSet)
          queue.enqueueAll(validNeighbours.map(_ :: path))
          visited.add(path)
        }
      }
      results

    bfs("start", "end")

  def part1(input: Input) =
    val onceOnly: VisitMaxFunction = (_) => 1
    traverse(input)(onceOnly).size

  def part2(input: Input) =
    val twiceFirstThenOnce: VisitMaxFunction = (freqs) => if freqs.values.exists(_ > 1) then 1 else 2
    traverse(input)(twiceFirstThenOnce).size
