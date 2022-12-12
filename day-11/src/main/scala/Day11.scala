import cats.data.State
import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day11 {

  def part1(input: String): Int =
    Day11Parser.parse(input).run(20).monkeys
      .map(_.inspections)
      .sorted
      .takeRight(2)
      .product
}

final case class Monkey(
                         name: Int,
                         items: Seq[Int],
                         action: Int => Int,
                         target: Int => Int,
                         inspections: Int
                       ) {

  override def toString: String =
    s"Monkey $name: ${items.mkString(", ")} (passes: $inspections)"
}

final case class Game(monkeys: Vector[Monkey]) {

  override def toString: String =
    monkeys.mkString("\n")

  def run(rounds: Int): Game =
    List.fill(rounds)(Game.round).sequence
      .runS(this).value
}

object Game {

  def getMonkey(i: Int): State[Game, Monkey] =
    State.inspect(_.monkeys(i))

  def getItems(m: Int): State[Game, Seq[Int]] =
    getMonkey(m).map(_.items)

  def getItem(m: Int, i: Int): State[Game, Int] =
    getItems(m).map(_(i))

  def updateMonkey(i: Int, m: Monkey): State[Game, Unit] =
    State.modify { game =>
      game.copy(game.monkeys.updated(i, m))
    }

  def updateItems(m: Int, items: Seq[Int]): State[Game, Unit] =
    for {
      monkey <- getMonkey(m)
      _      <- updateMonkey(m, monkey.copy(items = items))
    } yield ()

  def updateItem(m: Int, i: Int, item: Int): State[Game, Unit] =
    for {
      oldItems <- getItems(m)
      _        <- updateItems(m, oldItems.updated(i, item))
    } yield ()

  def pass(from: Int, to: Int): State[Game, Unit] =
    for {
      fromItems <- getItems(from)
      toItems   <- getItems(to)
      _         <- updateItems(from, fromItems.tail)
      _         <- updateItems(to, toItems :+ fromItems.head)
    } yield ()

  def inspect(m: Int): State[Game, Unit] =
    for {
      monkey <- getMonkey(m)
      item   <- getItem(m, 0)
      _      <- updateItem(m, 0, monkey.action(item) / 3)
      monkey <- getMonkey(m)
      _      <- updateMonkey(m, monkey.copy(inspections = monkey.inspections + 1))
    } yield ()

  def turn(m: Int): State[Game, Unit] =
    (for {
      monkey <- getMonkey(m)
      _      <- inspect(m)
      worry  <- getItem(m, 0)
      to     =  monkey.target(worry)
      _      <- pass(m, to)
    } yield ()).whileM_(getItems(m).map(_.nonEmpty))

  def round: State[Game, Unit] =
    State.get.flatMap { game =>
      game.monkeys.indices.toList.traverse(turn).void
    }
}

object Day11Parser {

  import atto._
  import Atto._

  private val newline = char('\r') | char('\n')
  private val tab = char('\t') | manyN(2, char(' '))

  private val name =
    token(string("Monkey")) ~> int <~ char(':')

  private val items =
    tab ~>
      token(string("Starting items:")) ~>
      int.sepBy(token(char(',')))
        .map(_.toVector)

  private val expression = {
    val operand = int.map(Function.const[Int, Int]) | string("old").as(identity[Int] _)
    val multiply = char('*').as((a: Int, b: Int) => a * b)
    val plus = char('+').as((a: Int, b: Int) => a + b)
    val operator = multiply | plus
    (token(operand), token(operator), operand).mapN { (a, op, b) =>
      (old: Int) =>
        op(a(old), b(old))
    }
  }

  private val operation =
    tab ~>
      token(string("Operation: new =")) ~>
      expression

  private val action =
    (
      tab ~> token(string("Test: divisible by")) ~> int <~ newline,
      manyN(2, tab) ~> token(string("If true: throw to monkey")) ~> int <~ newline,
      manyN(2, tab) ~> token(string("If false: throw to monkey")) ~> int
    ).mapN { (test, ifTrue, ifFalse) =>
      (worry: Int) =>
        if (worry % test == 0) ifTrue else ifFalse
    }

  private val monkey = (
    name <~ newline,
    items <~ newline,
    operation <~ newline,
    action, ok(0)
  ).mapN(Monkey)

  private val game =
    monkey
      .sepBy(manyN(2, newline))
      .map(m => Game(m.toVector))

  def parse(input: String): Game =
    game.parseOnly(input).done.option.get
}