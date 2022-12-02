import cats.data.NonEmptyList
import cats.implicits._

object Day1 {

  def mostCalories(input: String): Int =
    mostCalories(Parser.parse(input))

  def mostCalories(input: NonEmptyList[NonEmptyList[Int]]): Int =
    input.map(_.sumAll).maximum

  def sumOfTop3MostCalories(input: String): Int =
    sumOfTop3MostCalories(Parser.parse(input))

  def sumOfTop3MostCalories(input: NonEmptyList[NonEmptyList[Int]]): Int =
    input.map(_.sumAll).sortBy(-_).take(3).sum
}

object Parser {

  import atto._
  import Atto._

  private val newline = char('\r') | char('\n')

  def parse(input: String): NonEmptyList[NonEmptyList[Int]] =
    parser.parseOnly(input).option.get

  private val parser: Parser[NonEmptyList[NonEmptyList[Int]]] =
    int.sepBy1(newline).sepBy1(manyN(2, newline))
}
