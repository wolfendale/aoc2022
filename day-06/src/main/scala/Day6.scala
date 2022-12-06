import cats.implicits._
import atto._
import Atto._

object Day6 {

  def part1(input: String): Int =
    parser.parseOnly(input).option.get

  private val marker: Parser[Int] =
    manyN(4, letter).flatMap { letters =>
      if (letters.toSet.size == 4) pos else err("letters weren't the same")
    }

  private val parser: Parser[Int] =
    many(marker.map(_.some) | skip(_ => true).as(None)).map(_.flatten.head)
}
