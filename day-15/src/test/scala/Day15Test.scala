import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day15Test extends AnyFreeSpec with Matchers {

  private val testInput = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                            |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                            |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                            |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                            |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                            |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                            |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                            |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                            |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                            |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                            |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                            |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                            |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                            |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day15.part1(testInput, 10) mustEqual 26
    }

    "must work for the puzzle input" in {
      Day15.part1(puzzleInput, 2000000) mustEqual 4876693
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day15.part2(testInput, 20) mustEqual 56000011
    }

    "must work for the puzzle input" in {
      Day15.part2(puzzleInput, 4000000) mustEqual 0
    }
  }
}
