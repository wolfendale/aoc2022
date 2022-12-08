import scala.annotation.tailrec

object Day8 {

  def part1(input: String): Int = {
    val forest = Day8Parser.parse(input)
    forest.zipWithIndex.foldLeft(Vector.empty[(Int, Int)]) { case (m, (row, y)) =>
      row.zipWithIndex.foldLeft(m) { case (m, (tree, x)) =>
        val column = forest.map(_(x))
        def visibleAlongAxis(axis: Vector[Int], position: Int): Boolean =
          axis.take(position).maxOption.forall(tree > _) ||
            axis.drop(position + 1).maxOption.forall(tree > _)
        if (visibleAlongAxis(row, x) || visibleAlongAxis(column, y)) m :+ (x, y) else m
      }
    }.length
  }

  def part2(input: String): Int = {
    val rows = Day8Parser.parse(input)
    val columns = rows.transpose
    rows.zipWithIndex.foldLeft(Vector.empty[Int]) { case (m, (row, y)) =>
      row.zipWithIndex.foldLeft(m) { case (m, (tree, x)) =>

        val column = columns(x)

        @tailrec
        def scoreForDirection(direction: Vector[Int], score: Int = 0): Int =
          direction.headOption match {
            case Some(a) if a >= tree => score + 1
            case Some(_)              => scoreForDirection(direction.tail, score + 1)
            case None                 => score
          }

        def scoreForAxis(axis: Vector[Int], position: Int): Int =
          scoreForDirection(axis.take(position).reverse) *
            scoreForDirection(axis.drop(position + 1))

        m :+ scoreForAxis(row, x) * scoreForAxis(column, y)
      }
    }
  }.max
}

object Day8Parser {

  import atto._
  import Atto._

  private val tree: Parser[Int] = digit.map(_.asDigit)

  private val newline: Parser[Char] = char('\n') | char('\r')

  private val parser: Parser[Vector[Vector[Int]]] =
    many(tree).map(_.toVector)
      .sepBy(newline).map(_.toVector)

  def parse(input: String): Vector[Vector[Int]] =
    parser.parseOnly(input).done.option.get
}
