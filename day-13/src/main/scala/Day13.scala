import cats.implicits._

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.chaining.scalaUtilChainingOps

object Day13 {

  def part1(input: String): Int = {
    Day13Parser.parsePairs(input).zipWithIndex.collect {
      case ((a, b), i) if a < b => i + 1
    }.sum
  }

  private val dividerPackets: Seq[Packet] =
    Seq(
      Packet.L(List(Packet.L(List(Packet.N(2))))),
      Packet.L(List(Packet.L(List(Packet.N(6)))))
    )

  def part2(input: String): Int = {
    (Day13Parser.parsePackets(input) ++ dividerPackets)
      .sorted
      .zipWithIndex
      .collect {
        case (packet, i) if dividerPackets.contains(packet) => i + 1
      }.product
  }
}

sealed trait Packet extends Product with Serializable

object Packet {

  final case class N(value: Int) extends Packet {
    override lazy val toString: String = value.toString
  }

  final case class L(values: List[Packet]) extends Packet {
    override lazy val toString: String =
      s"[${values.mkString(",")}]"
  }

  implicit lazy val ordering: Ordering[Packet] = {
    case (Packet.N(a), Packet.N(b)) => a compare b
    case (a: Packet.L, b: Packet.L) => compareLists(a, b)
    case (a: Packet.N, b: Packet.L) => ordering.compare(Packet.L(List(a)), b)
    case (a: Packet.L, b: Packet.N) => ordering.compare(a, Packet.L(List(b)))
  }

  private def compareLists(a: Packet.L, b: Packet.L): Int =
    a.values.map(_.some).zipAll(b.values.map(_.some), None, None)
      .foldLeft(0) {
        case (m, (_, _)) if m != 0   => m
        case (_, (Some(_), None))    => 1
        case (_, (None, Some(_)))    => -1
        case (_, (None, None))       => 0
        case (_, (Some(a), Some(b))) => ordering.compare(a, b)
      }
}

object Day13Parser {

  import atto._
  import Atto._

  private val newline = char('\n') | char('\r')

  private val n: Parser[Packet.N] =
    int.map(Packet.N)
  private lazy val l: Parser[Packet.L] =
    (char('[') ~> packet.sepBy(char(',')) <~ char(']')).map(Packet.L)
  private lazy val packet: Parser[Packet] =
    n | l

  private val pairParser =
    pairBy(packet, newline, packet).sepBy(manyN(2, newline))

  def parsePairs(input: String): List[(Packet, Packet)] =
    pairParser.parseOnly(input).done.option.get

  def parsePackets(input: String): Seq[Packet] =
    packet.sepBy(manyN(2, newline) | newline)
      .parseOnly(input).done.option.get
}
