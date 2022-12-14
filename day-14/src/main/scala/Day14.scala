import cats.data.State
import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day14 {

  def part1(input: String): Int =
    Day14Parser.parse(input).run
      .tap(println)
      .stoppedSand.size

  def part2(input: String): Int =
    Day14Parser.parse(input).withFloor.run
      .tap(println)
      .stoppedSand.size
}

final case class Vec2(x: Int, y: Int) {

  def +(other: Vec2): Vec2 =
    Vec2(x + other.x, y + other.y)

  def to(other: Vec2): Seq[Vec2] =
    for {
      x <- if (x < other.x) x to other.x else other.x to x
      y <- if (y < other.y) y to other.y else other.y to y
    } yield Vec2(x, y)
}

final case class Line(vertices: Seq[Vec2]) {

  def points: Seq[Vec2] =
    vertices.sliding(2).foldLeft(Seq.empty[Vec2]) {
      case (points, List(a, b)) =>
        points ++ (a to b)
    }
}

sealed trait Tile extends Product with Serializable

object Tile {

  case object Rock extends Tile { override def toString: String = "#" }
  case object Sand extends Tile { override def toString: String = "o" }
}

final case class Simulation(rocks: Set[Vec2], fallingSand: Set[Vec2], stoppedSand: Set[Vec2], floor: Option[Int] = None) {

  def withFloor: Simulation =
    copy(floor = Some(bottom + 2))

  lazy val run: Simulation =
    Simulation.run.runS(this).value

  private lazy val bottom: Int = rocks.map(_.y).max

  private lazy val map = (
    rocks.map(_ -> Tile.Rock) ++
      (fallingSand ++ stoppedSand).map(_ -> Tile.Sand)
    ).toMap

  override def toString: String = {
    val points = map.keys.toSeq
    val minX = points.minBy(_.x).x
    val maxX = points.maxBy(_.x).x
    val minY = points.minBy(_.y).y
    val maxY = points.maxBy(_.y).y
    (minY to maxY).map { y =>
      (minX to maxX).map { x =>
        map.get(Vec2(x, y)).map(_.toString).getOrElse(".")
      }.mkString
    }.mkString("\n")
  }
}

object Simulation {

  private val floor: State[Simulation, Option[Int]] =
    State.inspect(_.floor)

  private val rocks: State[Simulation, Set[Vec2]] =
    State.inspect(_.rocks)

  private val fallingSand: State[Simulation, Set[Vec2]] =
    State.inspect(_.fallingSand)

  private val stoppedSand: State[Simulation, Set[Vec2]] =
    State.inspect(_.stoppedSand)

  private val bottom: State[Simulation, Int] =
    State.inspect(_.bottom)

  private val spawn: State[Simulation, Unit] =
    State.modify { s =>
      s.copy(fallingSand = s.fallingSand + Vec2(500, 0))
    }

  private def existsAt(point: Vec2): State[Simulation, Boolean] =
    for {
      floor <- floor
      rocks <- rocks
      sand  <- stoppedSand
    } yield floor.exists(point.y >= _) || rocks.contains(point) || sand.contains(point)

  private def move(a: Vec2, b: Vec2): State[Simulation, Unit] =
    State.modify { s =>
      s.copy(fallingSand = s.fallingSand - a + b)
    }

  private def stop(block: Vec2): State[Simulation, Unit] =
    State.modify { s =>
      s.copy(fallingSand = s.fallingSand - block, stoppedSand = s.stoppedSand + block)
    }

  private def fall(block: Vec2): State[Simulation, Unit] =
    existsAt(block + Vec2(0, 1)).ifM(
      existsAt(block + Vec2(-1, 1)).ifM(
        existsAt(block + Vec2(1, 1)).ifM(
          stop(block),
          move(block, block + Vec2(1, 1))
        ),
        move(block, block + Vec2(-1, 1))
      ),
      move(block, block + Vec2(0, 1))
    )

  private val finished: State[Simulation, Boolean] =
    for {
      floor   <- floor
      bottom  <- bottom
      falling <- fallingSand
      stopped <- stoppedSand
    } yield floor.as {
      stopped.contains(Vec2(500, 0))
    }.getOrElse(falling.exists(_.y > bottom))

  private val fall: State[Simulation, Unit] =
    fallingSand.flatMap { blocks =>
      blocks.toList.traverse(fall(_))
    }.void

  private val step: State[Simulation, Unit] =
    fallingSand.map(_.isEmpty).ifM(spawn, fall)

  private val run: State[Simulation, Unit] =
    step.untilM_(finished)

  def from(lines: Seq[Line]): Simulation =
    lines.foldLeft(Set.empty[Vec2]) { (m, line) =>
      m ++ line.points
    }.pipe(Simulation(_, Set.empty, Set.empty))
}

object Day14Parser {

  import atto._
  import Atto._

  private val newline = char('\r') | char('\n')

  private val point = pairBy(int, char(','), int).map(Vec2.tupled)
  private val line = point.sepBy(horizontalWhitespace ~> token(string("->"))).map(Line)

  private val parser = line.sepBy(newline).map(Simulation.from)

  def parse(input: String): Simulation =
    parser.parseOnly(input).done.option.get
}