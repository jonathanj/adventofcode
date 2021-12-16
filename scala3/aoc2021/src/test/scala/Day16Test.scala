import org.junit.Assert.*
import org.junit.Test

import scala.io.Source

class Day16Test:
  @Test def t1(): Unit =
    val solve = Day16.parse andThen Day16.part1
    assertEquals(16, solve("8A004A801A8002F478"))
    assertEquals(12, solve("620080001611562C8802118E34"))
    assertEquals(23, solve("C0015000016115A2E0802F182340"))
    assertEquals(31, solve("A0016C880162017C3686B18A3D4780"))
    assertEquals(993, solve(Source.fromResource("day16").mkString))

  @Test def t2(): Unit =
    val solve = Day16.parse andThen Day16.part2
    assertEquals(BigInt(3), solve("C200B40A82"))
    assertEquals(BigInt(54), solve("04005AC33890"))
    assertEquals(BigInt(7), solve("880086C3E88112"))
    assertEquals(BigInt(9), solve("CE00C43D881120"))
    assertEquals(BigInt(1), solve("D8005AC2A8F0"))
    assertEquals(BigInt(0), solve("F600BC2D8F"))
    assertEquals(BigInt(0), solve("9C005AC2F8F0"))
    assertEquals(BigInt(1), solve("9C0141080250320F1802104A08"))
    assertEquals(BigInt("144595909277"), solve(Source.fromResource("day16").mkString))
