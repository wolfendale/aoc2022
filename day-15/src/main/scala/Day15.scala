import cats.implicits._

object Day15 {

  def part1(input: String, y: Int): Int = {
    val sensors = Day15Parser.parse(input)
    val beacons = sensors.map(_.closestBeacon).toSet
    val minX = sensors.map(s => s.point.x - s.range).min
    val maxX = sensors.map(s => s.point.x + s.range).max
    (minX to maxX).toList.count { x =>
      val point = Vec2(x, y)
      sensors.exists { s =>
        point.manhattanDistance(s.point) <= s.range &&
          !beacons.contains(point)
      }
    }
  }

  def part2(input: String, max: Int): Int = {

    val sensors = Day15Parser.parse(input)
    val perimeters = sensors.map(_.perimeter)

    val points = for {
      p1 <- perimeters
      p2 <- perimeters.filterNot(_ eq p1)
      intersection <- p1 & p2
    } yield intersection

    val point = points
      .find { p =>
        p.x >= 0 &&
          p.x < max &&
          p.y >= 0 &&
          p.y < max &&
          sensors.forall(s => s.point.manhattanDistance(p) > s.range)
      }.get

    (point.x * 4000000) + point.y
  }
}

final case class Vec2(x: Int, y: Int) {

  def manhattanDistance(other: Vec2): Int = {
    val point = (this - other).abs
    point.x + point.y
  }

  def -(other: Vec2): Vec2 =
    Vec2(x - other.x, y - other.y)

  def +(other: Vec2): Vec2 =
    Vec2(x + other.x, y + other.y)

  def abs: Vec2 =
    Vec2(x.abs, y.abs)
}

final case class Sensor(point: Vec2, closestBeacon: Vec2) {

  val range: Int = point.manhattanDistance(closestBeacon)

  lazy val perimeter: Set[Vec2] = {
    for {
      x  <- -range - 1 to range + 1
      y  =  range - x.abs + 1
      ys <- Seq(y, -y)
    } yield Vec2(x, ys) + point
  }.toSet
}

object Day15Parser {

  import atto._
  import Atto._

  private val newline = char('\n') | char('\r')
  private val point = pairBy(token(string("x=") ~> int), token(char(',')), string("y=") ~> int).map(Vec2.tupled)
  private val line = pairBy(token(string("Sensor at")) ~> point, token(char(':')), token(string("closest beacon is at")) ~> point).map(Sensor.tupled)
  private val parser = line.sepBy(newline)

  def parse(input: String): List[Sensor] =
    parser.parseOnly(input).done.option.get
}
