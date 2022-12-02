import cats._
import cats.implicits._
import cats.data.NonEmptyList

object Day2 {

  def score(input: String): Int =
    score(Parser.parse(input))

  def score(input: NonEmptyList[Step]): Int =
    input.map(_.score).sumAll
}

object Parser {

  import atto._
  import Atto._

  def parse(input: String): NonEmptyList[Step] =
    parser.parseOnly(input).option.get

  private val theirMove: Parser[Move] =
    char('A') >| Move.Rock |
    char('B') >| Move.Paper |
    char('C') >| Move.Scissors

  private val yourMove: Parser[Move] =
    char('X') >| Move.Rock |
    char('Y') >| Move.Paper |
    char('Z') >| Move.Scissors

  private val newline: Parser[Char] = char('\r') | char('\n')

  private val parser: Parser[NonEmptyList[Step]] =
    (theirMove ~ (whitespace ~> yourMove)).map(Step.tupled).sepBy1(newline)
}