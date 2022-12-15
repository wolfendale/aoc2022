import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day15 {

  def part1(input: String, y: Int): Int = {
    val sensors = Day15Parser.parse(input)
    val beacons = sensors.map(_.closestBeacon).toSet
    val minX = sensors.map(s => s.point.x - s.range).min
    val maxX = sensors.map(s => s.point.x + s.range).max
    (minX.toInt to maxX.toInt).toList.count { x =>
      val point = Vec2(x, y)
      sensors.exists { s =>
        point.manhattanDistance(s.point) <= s.range &&
          !beacons.contains(point)
      }
    }
  }
}

final case class Vec2(x: Double, y: Double) {

  def manhattanDistance(other: Vec2): Double =
    (this - other).abs.pipe(p => p.x + p.y)

  def -(other: Vec2): Vec2 =
    Vec2(x - other.x, y - other.y)

  def abs: Vec2 =
    Vec2(x.abs, y.abs)
}

final case class LineSegment(a: Vec2, b: Vec2)

final case class Sensor(point: Vec2, closestBeacon: Vec2) {

  val range: Double = point.manhattanDistance(closestBeacon)
}

object Day15Parser {

  import atto._
  import Atto._

  private val newline = char('\n') | char('\r')
  private val point = pairBy(token(string("x=") ~> double), token(char(',')), string("y=") ~> double).map(Vec2.tupled)
  private val line = pairBy(token(string("Sensor at")) ~> point, token(char(':')), token(string("closest beacon is at")) ~> point).map(Sensor.tupled)
  private val parser = line.sepBy(newline)

  def parse(input: String): List[Sensor] =
    parser.parseOnly(input).done.option.get
}
