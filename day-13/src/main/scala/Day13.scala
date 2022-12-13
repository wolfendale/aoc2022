import cats.implicits._

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.chaining.scalaUtilChainingOps

object Day13 {

  def part1(input: String): Int = {
    Day13Parser.parse(input).zipWithIndex.flatMap {
      case ((a, b), i) if a < b => Some(i + 1)
      case _                    => None
    }.sum
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
        case (_, (Some(_), None))    => 1 // not sure if this is right
        case (_, (None, Some(_)))    => -1 // not sure if this is right
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

  private val parser =
    pairBy(packet, newline, packet).sepBy(manyN(2, newline))

  def parse(input: String): List[(Packet, Packet)] =
    parser.parseOnly(input).done.option.get
}
