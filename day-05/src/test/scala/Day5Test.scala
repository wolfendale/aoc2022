import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day5Test extends AnyFreeSpec with Matchers {

  private val testInput = """    [D]
                            |[N] [C]
                            |[Z] [M] [P]
                            | 1   2   3
                            |
                            |move 1 from 2 to 1
                            |move 3 from 1 to 3
                            |move 2 from 2 to 1
                            |move 1 from 1 to 2""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must return x" in {
      Day5.part1(testInput) mustEqual "CMZ"
    }

    "result" in {
      Day5.part1(input) mustEqual "TDCHVHJTG"
    }
  }
}
