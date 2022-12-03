import cats._
import cats.implicits._
import cats.data.NonEmptyList

import scala.util.chaining.scalaUtilChainingOps

object Day3 {

  def part1(input: String): Int =
    part1(Parser.parse(input))

  def part1(input: NonEmptyList[Rucksack]): Int =
    input.map { rucksack =>
      (rucksack.firstCompartment.toSet & rucksack.secondCompartment.toSet)
        .head
        .pipe(values)
    }.sumAll

  private val values = ('a' to 'z').zip(1 to 26).toMap ++
    ('A' to 'Z').zip(27 to 52).toMap
}

object Parser {

  import atto._
  import Atto._

  def parse(input: String): NonEmptyList[Rucksack] =
    parser.parseOnly(input).option.get

  private val newline =
    char('\r') | char('\n')

  private val parser =
    many(letter).sepBy1(newline).map { backpacks =>
      backpacks.map { items =>
        items
          .splitAt(items.length / 2)
          .pipe(Rucksack.tupled)
      }
    }
}

final case class Rucksack(firstCompartment: Seq[Char], secondCompartment: Seq[Char])
