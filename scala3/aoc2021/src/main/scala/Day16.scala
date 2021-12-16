import aoc2021.Util.hexToBinary

object Day16:
  enum PacketType:
    case Literal(value: BigInt)
    case Operator(op: Int, packets: List[Packet])
  case class Packet(version: Int, payload: PacketType)

  def parseLiteral(stream: Stream[Char]): (Int, BigInt) =
    def parse(stream: Stream[Char]): List[String] =
      stream.headOption match {
        case None => List()
        case Some('0') => List(stream.slice(1, 5).mkString)
        case Some(_) => stream.slice(1, 5).mkString :: parse(stream.drop(5))
      }
    val s = parse(stream)
    (s.length * 5, BigInt(s.mkString, 2))

  def parseOperator(stream: Stream[Char]): (Int, List[Packet]) =
    stream.head match {
      case '0' =>
        val len = Integer.parseInt(stream.slice(1, 16).mkString, 2)
        (16 + len, parsePacket(stream.slice(16, len + 16)))
      case '1' | _ =>
        val count = Integer.parseInt(stream.slice(1, 12).mkString, 2)
        (1 to count).foldLeft((12, List[Packet]())) { case ((n, packets), _) =>
          parseOnePacket(stream.drop(n)) match {
            case None => (n, packets)
            case Some((nn, npackets)) => (n + nn, packets ::: npackets)
          }
        }
    }

  def parseOnePacket(stream: Stream[Char]): Option[(Int, List[Packet])] =
    if (stream.length <= 7) {
      None
    } else {
      val version = Integer.parseInt(stream.take(3).mkString, 2)
      val typeID = Integer.parseInt(stream.slice(3, 6).mkString, 2)
      typeID match {
        case 4 =>
          val (n, value) = parseLiteral(stream.drop(6))
          Some((6 + n, List(Packet(version, PacketType.Literal(value)))))
        case _ =>
          val (n, packets) = parseOperator(stream.drop(6))
          Some((6 + n, List(Packet(version, PacketType.Operator(typeID, packets)))))
      }
    }

  def parsePacket(stream: Stream[Char]): List[Packet] =
    stream match {
      case Stream.Empty => List()
      case stream => parseOnePacket(stream) match {
        case None => List()
        case Some((n, packets)) => packets ::: parsePacket(stream.drop(n))
      }
    }

  def parse(input: String): Packet =
    parsePacket(hexToBinary(input).toStream).head

  def part1(root: Packet): Int =
    def sumVersions(packets: List[Packet]): Int =
      packets match {
        case List() => 0
        case packet :: rest =>
          packet.payload match {
            case PacketType.Literal(_) => packet.version + sumVersions(rest)
            case PacketType.Operator(_, packets) => packet.version + sumVersions(packets ::: rest)
          }
      }
    sumVersions(List(root))

  def part2(root: Packet): BigInt =
    def binaryOp(op: (BigInt, BigInt) => BigInt)(operands: List[BigInt]): BigInt =
      operands match {
        case List(a, b) => op(a, b)
        case _ => throw new Exception("Malformed operator")
      }

    val operation: Int => List[BigInt] => BigInt = {
      case 0 => _.sum
      case 1 => _.product
      case 2 => _.min
      case 3 => _.max
      case 5 => binaryOp((a, b) => if a > b  then 1 else 0)
      case 6 => binaryOp((a, b) => if a < b  then 1 else 0)
      case 7 => binaryOp((a, b) => if a == b then 1 else 0)
    }

    def interpret(packet: Packet): BigInt =
      packet.payload match {
        case PacketType.Literal(value) => value
        case PacketType.Operator(op, packets) => operation(op)(packets.map(interpret))
      }

    interpret(root)
