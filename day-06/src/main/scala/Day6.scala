import cats.implicits._
import atto._
import Atto._

object Day6 {

  def part1(input: String): Int =
    parser(4).parseOnly(input).option.get

  def part2(input: String): Int =
    parser(14).parseOnly(input).option.get

  private def marker(size: Int): Parser[Int] =
    manyN(size, letter).flatMap { letters =>
      if (letters.toSet.size == size) pos else err("letters weren't the same")
    }

  private def parser(markerSize: Int): Parser[Int] =
    many(marker(markerSize).map(_.some) | skip(_ => true).as(None)).map(_.flatten.head)
}
