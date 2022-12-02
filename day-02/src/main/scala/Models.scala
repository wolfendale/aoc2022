import Part1.Parser
import atto.Parser
import cats.data.NonEmptyList

sealed abstract class Move(val score: Int) {
  def result(other: Move): Result
  def moveForResult(result: Result): Move
}

object Move {

  case object Rock extends Move(1) {

    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Draw
        case Paper    => Result.Loss
        case Scissors => Result.Win
      }

    override def moveForResult(result: Result): Move =
      result match {
        case Result.Win  => Paper
        case Result.Loss => Scissors
        case Result.Draw => Rock
      }
  }

  case object Paper extends Move(2) {

    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Win
        case Paper    => Result.Draw
        case Scissors => Result.Loss
      }

    override def moveForResult(result: Result): Move =
      result match {
        case Result.Win  => Scissors
        case Result.Loss => Rock
        case Result.Draw => Paper
      }
  }

  case object Scissors extends Move(3) {

    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Loss
        case Paper    => Result.Win
        case Scissors => Result.Draw
      }

    override def moveForResult(result: Result): Move =
      result match {
        case Result.Win  => Rock
        case Result.Loss => Paper
        case Result.Draw => Scissors
      }
  }
}

sealed abstract class Result(val score: Int)

object Result {

  case object Win extends Result(6)
  case object Draw extends Result(3)
  case object Loss extends Result(0)
}

object Part1 {

  final case class Step(theirs: Move, yours: Move) {

    def score: Int =
      yours.score + yours.result(theirs).score
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
}

object Part2 {

  object Parser {

    import atto._
    import Atto._

    def parse(input: String): NonEmptyList[Step] =
      parser.parseOnly(input).option.get

    private val move: Parser[Move] =
      char('A') >| Move.Rock |
      char('B') >| Move.Paper |
      char('C') >| Move.Scissors

    private val result: Parser[Result] =
      char('X') >| Result.Loss |
      char('Y') >| Result.Draw |
      char('Z') >| Result.Win

    private val newline: Parser[Char] = char('\r') | char('\n')

    private val parser: Parser[NonEmptyList[Step]] =
      (move ~ (whitespace ~> result)).map(Step.tupled).sepBy1(newline)
  }

  final case class Step(move: Move, result: Result) {

    def score: Int =
      result.score + move.moveForResult(result).score
  }
}