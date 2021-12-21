import aoc2021.Util.{stringLines, divmod}
import scala.annotation.tailrec
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.{Parser => P}

// Notes:
// - Zippers or lenses could make manipulating and traversing the tree much simpler.
// - Using linked lists to represent the pairs, tracking depth manually, could be simpler than trees.

object Day18:
  enum Tree:
    case Leaf(var value: Int)
    case Node(var left: Tree, var right: Tree)

    override def toString: String =
      this match {
        case Tree.Leaf(value) => s"${value}"
        case Tree.Node(l, r) => s"[${l},${r}]"
      }

  type Pair = (Any, Any)
  // Grammar to parse the string input into a hierarchy of pairs.
  val pairs = P.recursive[Pair] { recurse =>
    val elem = digit.rep.string.map(_.toInt) | recurse
    ((elem <* P.char(',')) ~ elem).between(P.char('['), P.char(']'))
  }

  def parseOne(input: String): Tree.Node =
    def nodes(tree: Pair): Tree =
      val left = tree._1 match {
        case i: Int => Tree.Leaf(i)
        case subtree: Pair => nodes(subtree)
      }
      val right = tree._2 match {
        case i: Int => Tree.Leaf(i)
        case subtree: Pair => nodes(subtree)
      }
      Tree.Node(left, right)

    pairs.parse(input) match {
      case Right((remaining, tree)) => remaining match {
        case "" => nodes(tree) match {
          case root @ Tree.Node(_, _) => root
          case _ => throw new Error("Malformed tree")
        }
        case _ => throw new Error(s"Remaining input: ${remaining}")
      }
      case _ => throw new Exception("Parsing error")
    }

  def parse(input: String): Seq[Tree.Node] =
    stringLines(input.trim).map(parseOne)

  def inorder(root: Tree): List[Tree.Node] =
    root match {
      case Tree.Leaf(_) => List()
      case root @ Tree.Node(l, r) => inorder(l) ::: (root :: inorder(r))
    }

  /**
   * Travel all the way up the tree until a new branch on `side` is available,
   * then take it and keep to the `alt` side until we find a leaf.
   */
  def nearestSide(side: Tree.Node => Tree, alt: Tree.Node => Tree)(node: Tree.Node, path: List[Tree]): Option[Tree.Leaf] =
    @tailrec
    def keepTo(dir: Tree.Node => Tree)(node: Tree): Tree.Leaf =
      node match {
        case (n: Tree.Node) => keepTo(dir)(dir(n))
        case (n: Tree.Leaf) => n
      }

    @tailrec
    def recur(queue: List[Tree], from: Tree): Option[Tree.Leaf] =
      queue match {
        case List() => None
        case (n: Tree.Node) :: rest if side(n) ne from => Some(keepTo(alt)(side(n)))
        case (n: Tree.Leaf) :: _ => Some(n)
        case n :: rest => recur(rest, n)
      }
    recur(path, node)

  def nearestLeft = nearestSide(_.left, _.right)
  def nearestRight = nearestSide(_.right, _.left)

  def findExploding(node: Tree): Option[List[Tree]] =
    def _find(node: Tree, depth: Int, path: List[Tree]): Option[List[Tree]] =
      node match {
        case Tree.Leaf(_) => None
        case node @ Tree.Node(_, _) if depth == 4 => Some(path)
        case node @ Tree.Node(l, r) => _find(l, depth + 1, l :: path).orElse { _find(r, depth + 1, r :: path) }
      }
    _find(node, 0, List(node))

  def explode(root: Tree.Node): Boolean =
    findExploding(root) match {
      case None => false
      case Some(path) =>
        path match {
          case (node @ Tree.Node(Tree.Leaf(lvalue), Tree.Leaf(rvalue))) :: _ =>
            nearestLeft(node, path.tail).foreach(_.value += lvalue)
            nearestRight(node, path.tail).foreach(_.value += rvalue)
            path.tail.head match {
              case parent @ Tree.Node(l, _) if l == node => parent.left = Tree.Leaf(0)
              case parent @ Tree.Node(_, r) if r == node => parent.right = Tree.Leaf(0)
              case _ => throw new Exception("Oops again")
            }
            true
          case _ => throw new Exception("Oops")
        }
    }

  def split(root: Tree.Node): Boolean =
    def splitValue(value: Int) =
      divmod(value, 2) match {
        case (a, 0) => (a, a)
        case (a, 1) => (a, a + 1)
      }

    for (node <- inorder(root)) {
      node match {
        case node @ Tree.Node(Tree.Leaf(value), _) if value >= 10 =>
          val (l, r) = splitValue(value)
          node.left = Tree.Node(Tree.Leaf(l), Tree.Leaf(r))
          return true
        case node @ Tree.Node(_, Tree.Leaf(value)) if value >= 10 =>
          val (l, r) = splitValue(value)
          node.right = Tree.Node(Tree.Leaf(l), Tree.Leaf(r))
          return true
        case _ =>
      }
    }
    false

  def reduce(root: Tree.Node): Tree.Node =
    while (true) {
      if (!explode(root) && !split(root)) {
        return root
      }
    }
    root

  def magnitude(node: Tree.Node): Long =
    node match {
      case Tree.Node(Tree.Leaf(a), Tree.Leaf(b)) => 3 * a            + 2 * b
      case Tree.Node(l: Tree.Node, Tree.Leaf(b)) => 3 * magnitude(l) + 2 * b
      case Tree.Node(Tree.Leaf(a), r: Tree.Node) => 3 * a            + 2 * magnitude(r)
      case Tree.Node(l: Tree.Node, r: Tree.Node) => 3 * magnitude(l) + 2 * magnitude(r)
    }

  def add(a: Tree.Node, b: Tree.Node): Tree.Node =
    // Making an immutable tree is hard, and probably a job for zippers or lenses, so cheat and copy the tree.
    def dupe[T <: Tree](node: T): T =
      node match {
        case node: Tree.Node => Tree.Node(dupe(node.left), dupe(node.right)).asInstanceOf[T]
        case node: Tree.Leaf => Tree.Leaf(node.value).asInstanceOf[T]
      }
    Tree.Node(dupe(a), dupe(b))

  def part1(numbers: Seq[Tree.Node]) =
    magnitude(numbers.tail.foldLeft(numbers.head) { (a, b) => reduce(add(a, b)) })

  def part2(numbers: Seq[Tree.Node]) =
    (for {
      a <- numbers
      b <- numbers
      if a != b
    } yield magnitude(reduce(add(a, b)))).max
