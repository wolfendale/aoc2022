import scala.util.chaining._
import cats.implicits._

object Day5 {

  def part1(input: String): String =
    Parse.parse(input)(CrateMover._9000).tops.mkString

  def part2(input: String): String =
    Parse.parse(input)(CrateMover._9001).tops.mkString
}

final case class Stacks(stacks: Vector[List[Char]]) {

  def tops: Seq[Char] = stacks.flatMap(_.headOption)

  def updated(index: Int, column: List[Char]): Stacks =
    copy(stacks.updated(index - 1, column))

  def column(index: Int): List[Char] =
    stacks(index - 1)
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
      (stacks: Stacks, mover: CrateMover) =>
        mover.move(stacks, times, from, to)
    }

  private val moves = move.sepBy1(newline)
    .map { operations =>
      operations.reduceLeft { (f, g) =>
        (stacks: Stacks, mover: CrateMover) =>
          g(f(stacks, mover), mover)
      }
    }

  private val parser =
    (stacks, manyN(2, newline) ~> moves).mapN { (stacks, operations) =>
      (mover: CrateMover) =>
        operations(stacks, mover)
    }

  def parse(input: String): CrateMover => Stacks =
    parser.parseOnly(input).option.get
}

sealed trait CrateMover {
  def move(stacks: Stacks, times: Int, from: Int, to: Int): Stacks
}

object CrateMover {

  case object _9000 extends CrateMover {

    def move(stacks: Stacks, from: Int, to: Int): Stacks =
      stacks
        .updated(from, stacks.column(from).tail)
        .updated(to, stacks.column(from).head :: stacks.column(to))

    override def move(stacks: Stacks, times: Int, from: Int, to: Int): Stacks =
      (0 until times).foldLeft(stacks) { (m, _) => move(m, from, to) }
  }

  case object _9001 extends CrateMover {

    override def move(stacks: Stacks, times: Int, from: Int, to: Int): Stacks =
      stacks
        .updated(from, stacks.column(from).drop(times))
        .updated(to, stacks.column(from).take(times) ::: stacks.column(to))
  }
}