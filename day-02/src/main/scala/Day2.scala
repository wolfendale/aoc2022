import atto.Parser
import cats._
import cats.implicits._
import cats.data.NonEmptyList

object Day2 {

  def part1(input: String): Int =
    part1(Part1.Parser.parse(input))

  def part1(input: NonEmptyList[Part1.Step]): Int =
    input.map(_.score).sumAll

  def part2(input: String): Int =
    part2(Part2.Parser.parse(input))

  def part2(input: NonEmptyList[Part2.Step]): Int =
    input.map(_.score).sumAll
}