import scala.annotation.tailrec

object Day9 {

  def part1(input: String): Int =
    simulate(input, 2)

  def part2(input: String): Int =
    simulate(input, 10)

  private def simulate(input: String, length: Int): Int = {
    val rope = Rope(List.fill(length)(Vec2(0, 0)))
    Day9Parser.parse(input).scanLeft(rope) { (m, n) =>
      n(m)
    }.map(_.segments.last).toSet.size
  }
}

final case class Vec2(x: Int, y: Int) {

  def +(other: Vec2): Vec2 =
    Vec2(x + other.x, y + other.y)

  def -(other: Vec2): Vec2 =
    Vec2(x - other.x, y - other.y)

  def abs: Vec2 =
    Vec2(x.abs, y.abs)

  def distance(other: Vec2): Double = {
    val h = (this - other).abs
    Math.sqrt(Math.pow(h.x, 2) + Math.pow(h.y, 2))
  }
}

final case class Rope(segments: List[Vec2]) {

  def move(direction: Vec2): Rope =
    segments match {
      case head :: tail =>
        val newHead = head + direction
        Rope(newHead :: simulate(newHead, tail))
    }

  @tailrec
  private def simulate(previous: Vec2, rest: List[Vec2], newRope: List[Vec2] = Nil): List[Vec2] =
    rest match {
      case head :: tail =>
        val newHead = if (head.distance(previous) > 1.5) {
          val direction = previous - head
          head + Vec2(clamp(-1, 1, direction.x), clamp(-1, 1, direction.y))
        } else head
        simulate(newHead, tail, newHead :: newRope)
      case Nil => newRope.reverse
    }

  private def clamp(min: Int, max: Int, value: Int): Int =
    if (value < min) min else if (value > max) max else value
}

object Day9Parser {

  import atto._
  import Atto._

  private val newline =
    char('\n') | char('\r')

  private def move(c: Char, direction: Vec2): Parser[Seq[Rope => Rope]] =
    (token(char(c)) ~> int).map { times =>
      (0 until times).map { _ =>
        (_: Rope).move(direction)
      }
    }

  private val up = move('U', Vec2(0, 1))
  private val down = move('D', Vec2(0, -1))
  private val left = move('L', Vec2(-1, 0))
  private val right = move('R', Vec2(1, 0))

  private val parser: Parser[Seq[Rope => Rope]] =
    (up | down | left | right)
      .sepBy(newline).map(_.flatten)

  def parse(input: String): Seq[Rope => Rope] =
    parser.parseOnly(input).done.option.get
}