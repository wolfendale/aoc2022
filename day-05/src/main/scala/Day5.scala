import scala.util.chaining._
import cats.implicits._

object Day5 {

  def part1(input: String): String =
    Parse.parse(input).tops.mkString
}

final case class Stacks(stacks: Vector[List[Char]]) {

  def move(from: Int, to: Int): Stacks = {
    copy(
      stacks
        .updated(from - 1, stacks(from - 1).tail)
        .updated(to - 1, stacks(from - 1).head :: stacks(to - 1))
    )
  }

  def move(times: Int, from: Int, to: Int): Stacks =
    (0 until times).foldLeft(this) { (m, _) => m.move(from, to) }

  def tops: Seq[Char] = stacks.flatMap(_.headOption)
}

object Parse {

  import atto._
  import Atto._

  private val crate = char('[') ~> letter <~ char(']')
  private val crateOrSpace = crate.map(_.some) | manyN(3, spaceChar).as(None)
  private val newline = char('\r') | char('\n')
  private val space = char(' ')
  private val number = space ~> digit <~ opt(space)
  private val numberLine = number.sepBy(space).map(_.length)

  private val stacks = (
      crateOrSpace
        .sepBy(spaceChar).map(_.toVector)
        .sepBy(newline).map(_.toVector),
      numberLine
    ).mapN { (data, columns) =>
      def get(col: Int, row: Int): Option[Char] =
        data.lift(row).flatMap(_.lift(col).flatten)
      (0 until columns).map { col =>
        data.indices.map { row =>
          get(col, row)
        }.toList.flatten
      }.toVector.pipe(Stacks)
    }

  private val move =
    (
      token(string("move")) ~> token(int),
      token(string("from")) ~> token(int),
      token(string("to")) ~> int
    ).mapN { (times, from, to) =>
      (stacks: Stacks) =>
        stacks.move(times, from, to)
    }

  private val moves = move.sepBy1(newline)
    .map(_.reduceLeft(_ andThen _))

  private val parser =
    (stacks, manyN(2, newline) ~> moves).mapN { (stacks, operations) =>
      operations(stacks)
    }

  def parse(input: String): Stacks =
    parser.parseOnly(input).option.get
}