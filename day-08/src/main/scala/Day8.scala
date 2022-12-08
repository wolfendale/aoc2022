

object Day8 {

  def part1(input: String): Long = {
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
