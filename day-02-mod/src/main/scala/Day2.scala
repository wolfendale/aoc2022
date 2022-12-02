import cats.data.NonEmptyList
import cats.implicits._
import Mod.ToMod

object Day2 extends App {

  def part1(input: String): Int =
    Parser.parse(input).map { case (theirs, yours) =>
      (yours.toInt + 1) + ((yours - theirs - 2.toMod(3)).toInt * 3)
    }.sumAll

  def part2(input: String): Int =
    Parser.parse(input).map { case (move, result) =>
      (result.toInt * 3) + (move + (result - 1.toMod(3))).toInt + 1
    }.sumAll
}

object Parser {

  import atto._
  import Atto._

  def parse(input: String): NonEmptyList[(Mod[3], Mod[3])] =
    parser.parseOnly(input).option.get

  private val first: Parser[Mod[3]] =
    char('A') >| 0.toMod(3) |
    char('B') >| 1.toMod(3) |
    char('C') >| 2.toMod(3)

  private val second: Parser[Mod[3]] =
    char('X') >| 0.toMod(3) |
    char('Y') >| 1.toMod(3) |
    char('Z') >| 2.toMod(3)

  private val newline: Parser[Char] = char('\r') | char('\n')

  private val parser: Parser[NonEmptyList[(Mod[3], Mod[3])]] =
    (first ~ (whitespace ~> second)).sepBy1(newline)
}

final class Mod[M <: Int] private (val value: Int, val mod: M) {

  def toInt: Int = value

  def +(other: Mod[mod.type]): Mod[mod.type] =
    ((value + other.value) % mod).toMod(mod)

  def -(other: Mod[mod.type]): Mod[mod.type] =
    ((value - other.value + mod) % mod).toMod(mod)
}

object Mod {

  def apply(value: Int, mod: Int): Mod[mod.type] =
    new Mod(value % mod, mod)

  implicit class ToMod(value: Int) {
    def toMod(mod: Int): Mod[mod.type] =
      Mod(value, mod)
  }
}

