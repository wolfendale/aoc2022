import cats._
import cats.implicits._
import cats.data.NonEmptyList

object Day4 {

  def part1(input: String): Long =
    Parser.parse(input).count { case (a, b) =>
      val c = a.toSet & b.toSet
      c == a.toSet || c == b.toSet
    }

  def part2(input: String): Long =
    Parser.parse(input).count { case (a, b) =>
      (a.toSet & b.toSet).nonEmpty
    }
}

object Parser {

  import atto._
  import Atto._

  private val newline =
    char('\r') | char('\n')

  private val range =
    (int ~ (char('-') ~> int))
      .map({ case (a, b) => a to b })

  private val parser =
    (range ~ (char(',') ~> range)).sepBy1(newline)

  def parse(input: String): NonEmptyList[(Range, Range)] =
    parser.parseOnly(input).option.get
}