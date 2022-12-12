import cats.data.State
import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day11 {

  def part1(input: String): Long =
    Day11Parser.parse(input)
      .withWorryReduction(_ / 3)
      .run(20)
      .pipe(monkeyBusinessLevel)

  def part2(input: String): Long = {
    val game = Day11Parser.parse(input)
    val mod = game.monkeys.map(_.divisibleBy).product
    game
      .withWorryReduction(_ % mod)
      .run(10000)
      .pipe(monkeyBusinessLevel)
  }

  private def monkeyBusinessLevel(game: Game): Long =
    game.monkeys
      .map(_.inspections)
      .sorted
      .takeRight(2)
      .product
}

final case class Monkey(
                         name: Int,
                         items: Seq[Long],
                         action: Long => Long,
                         divisibleBy: Int,
                         ifTrue: Int,
                         ifFalse: Int,
                         inspections: Long
                       ) {

  def target(worry: Long): Int =
    if (worry % divisibleBy == 0) ifTrue else ifFalse

  override def toString: String =
    s"Monkey $name inspected items $inspections times"
}

final case class Game(monkeys: Vector[Monkey], worryReduction: Long => Long = identity) {

  override def toString: String =
    monkeys.mkString("\n")

  def run(rounds: Int): Game =
    List.fill(rounds)(Game.round).sequence
      .runS(this).value

  def withWorryReduction(f: Long => Long): Game =
    copy(worryReduction = f)
}

object Game {

  def getMonkey(i: Int): State[Game, Monkey] =
    State.inspect(_.monkeys(i))

  def getItems(m: Int): State[Game, Seq[Long]] =
    getMonkey(m).map(_.items)

  def getItem(m: Int, i: Int): State[Game, Long] =
    getItems(m).map(_(i))

  def updateMonkey(i: Int, m: Monkey): State[Game, Unit] =
    State.modify { game =>
      game.copy(game.monkeys.updated(i, m))
    }

  def updateItems(m: Int, items: Seq[Long]): State[Game, Unit] =
    for {
      monkey <- getMonkey(m)
      _      <- updateMonkey(m, monkey.copy(items = items))
    } yield ()

  def updateItem(m: Int, i: Int, item: Long): State[Game, Unit] =
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
      monkey         <- getMonkey(m)
      item           <- getItem(m, 0)
      worryReduction <- State.inspect[Game, Long => Long](_.worryReduction)
      _              <- updateItem(m, 0, worryReduction(monkey.action(item)))
      monkey         <- getMonkey(m)
      _              <- updateMonkey(m, monkey.copy(inspections = monkey.inspections + 1))
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
      long.sepBy(token(char(',')))
        .map(_.toVector)

  private val expression = {
    val operand = long.map(Function.const[Long, Long]) | string("old").as(identity[Long] _)
    val multiply = char('*').as((a: Long, b: Long) => a * b)
    val plus = char('+').as((a: Long, b: Long) => a + b)
    val operator = multiply | plus
    (token(operand), token(operator), operand).mapN { (a, op, b) =>
      (old: Long) =>
        op(a(old), b(old))
    }
  }

  private val operation =
    tab ~>
      token(string("Operation: new =")) ~>
      expression

  private val divisibleBy =
    tab ~> token(string("Test: divisible by")) ~> int

  private val ifTrue =
    manyN(2, tab) ~> token(string("If true: throw to monkey")) ~> int

  private val ifFalse =
    manyN(2, tab) ~> token(string("If false: throw to monkey")) ~> int

  private val monkey = (
    name <~ newline,
    items <~ newline,
    operation <~ newline,
    divisibleBy <~ newline,
    ifTrue <~ newline,
    ifFalse, ok(0L)
  ).mapN(Monkey)

  private val game =
    monkey
      .sepBy(manyN(2, newline))
      .map(m => Game(m.toVector))

  def parse(input: String): Game =
    game.parseOnly(input).done.option.get
}