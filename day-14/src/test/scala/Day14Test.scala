import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day14Test extends AnyFreeSpec with Matchers {

  private val testInput = """498,4 -> 498,6 -> 496,6
                            |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day14.part1(testInput) mustEqual 24
    }

    "must work for the puzzle input" in {
      Day14.part1(puzzleInput) mustEqual 1513
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day14.part2(testInput) mustEqual 93
    }

    "must work for the puzzle input" in {
      Day14.part2(puzzleInput) mustEqual 22646
    }
  }
}
