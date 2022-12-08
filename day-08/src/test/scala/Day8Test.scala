import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day8Test extends AnyFreeSpec with Matchers {

  private val testInput: String = """30373
                                    |25512
                                    |65332
                                    |33549
                                    |35390""".stripMargin

  private val puzzleInput: String = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day8.part1(testInput) mustEqual 21
    }

    "must work for the puzzle input" in {
      Day8.part1(puzzleInput) mustEqual 1733
    }
  }
}
