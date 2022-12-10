import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day10 {

  def part1(input: String): Int = {
    val wantedCycles = Set(20, 60, 100, 140, 180, 220)
    run(input)
      .filter(c => wantedCycles.contains(c.cycle))
      .map(_.signalStrength)
      .sum
  }

  def part2(input: String): String =
    run(input).foldLeft("") { (m, n) =>
      val x = n.cycle % 40
      m + (if (x >= n.x && x <= n.x + 2) "#" else ".")
    }.grouped(40).mkString("\n").tap(println)

  private def run(input: String): Seq[Computer] =
    Day10Parser.parse(input)
      .scanLeft(Computer(0, 1)) { (computer, op) =>
        op(computer)
      }
      .groupBy(_.cycle)
      .values
      .flatMap(_.headOption)
      .toSeq
      .sortBy(_.cycle)
}

final case class Computer(cycle: Int, x: Int) {

  def tick: Computer =
    copy(cycle = cycle + 1)

  def addX(amount: Int): Computer =
    copy(x = x + amount)

  def signalStrength: Int =
    cycle * x
}

object Day10Parser {

  import atto._
  import Atto._

  private val newline = char('\n') | char('\r')

  private val noop = string("noop").as {
    Seq((c: Computer) => c.tick)
  }

  private val addx = (token(string("addx")) ~> int).map { amount =>
    Seq.fill(2)((c: Computer) => c.tick) :+
      ((c: Computer) => c.addX(amount))
  }

  private val parser =
    (noop | addx)
      .sepBy(newline)
      .map(_.flatten)

  def parse(input: String): Seq[Computer => Computer] =
    parser.parseOnly(input).done.option.get
}
