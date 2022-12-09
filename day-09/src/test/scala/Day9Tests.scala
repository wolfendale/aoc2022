import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day9Tests extends AnyFreeSpec with Matchers {

  private val testInput = """R 4
                            |U 4
                            |L 3
                            |D 1
                            |R 4
                            |D 1
                            |L 5
                            |R 2""".stripMargin

  private val testInput2 = """R 5
                             |U 8
                             |L 8
                             |D 3
                             |R 17
                             |D 10
                             |L 25
                             |U 20""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day9.part1(testInput) mustEqual 13
    }

    "must work for the puzzle input" in {
      Day9.part1(puzzleInput) mustEqual 5907
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day9.part2(testInput) mustEqual 1
    }

    "must work for the second test input" in {
      Day9.part2(testInput2) mustEqual 36
    }

    "must work for the puzzle input" in {
      Day9.part2(puzzleInput) mustEqual 0
    }
  }
}
