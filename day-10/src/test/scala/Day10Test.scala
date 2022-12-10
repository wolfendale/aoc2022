import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day10Test extends AnyFreeSpec with Matchers {

  private val testInput = Source.fromResource("test.txt").mkString
  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day10.part1(testInput) mustEqual 13140
    }

    "must work for the puzzle input" in {
      Day10.part1(puzzleInput) mustEqual 0
    }
  }
}
