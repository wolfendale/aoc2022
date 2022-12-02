import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day2Test extends AnyFreeSpec with Matchers {

  private val testInput = """A Y
                    |B X
                    |C Z""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must return the score of the strategy guide" in {
      Day2.part1(testInput) mustEqual 15
    }

    "result" in {
      Day2.part1(input) mustEqual 10816
    }
  }

  "part 2" - {

    "must return the score of the strategy guide" in {
      Day2.part2(testInput) mustEqual 12
    }

    "result" in {
      Day2.part2(input) mustEqual 0
    }
  }
}
