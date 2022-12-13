import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day13Test extends AnyFreeSpec with Matchers {

  private val testInput = """[1,1,3,1,1]
                            |[1,1,5,1,1]
                            |
                            |[[1],[2,3,4]]
                            |[[1],4]
                            |
                            |[9]
                            |[[8,7,6]]
                            |
                            |[[4,4],4,4]
                            |[[4,4],4,4,4]
                            |
                            |[7,7,7,7]
                            |[7,7,7]
                            |
                            |[]
                            |[3]
                            |
                            |[[[]]]
                            |[[]]
                            |
                            |[1,[2,[3,[4,[5,6,7]]]],8,9]
                            |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day13.part1(testInput) mustEqual 13
    }

    "must work for the puzzle input" in {
      Day13.part1(puzzleInput) mustEqual 5605
    }
  }
}
