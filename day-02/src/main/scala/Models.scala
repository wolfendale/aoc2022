sealed abstract class Move(val score: Int) {
  def result(other: Move): Result
}

object Move {

  case object Rock extends Move(1) {
    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Draw
        case Paper    => Result.Loss
        case Scissors => Result.Win
      }
  }

  case object Paper extends Move(2) {
    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Win
        case Paper    => Result.Draw
        case Scissors => Result.Loss
      }
  }

  case object Scissors extends Move(3) {
    override def result(other: Move): Result =
      other match {
        case Rock     => Result.Loss
        case Paper    => Result.Win
        case Scissors => Result.Draw
      }
  }
}

sealed abstract class Result(val score: Int)

object Result {

  case object Win extends Result(6)
  case object Draw extends Result(3)
  case object Loss extends Result(0)
}

final case class Step(theirs: Move, yours: Move) {

  def score: Int =
    yours.score + yours.result(theirs).score
}