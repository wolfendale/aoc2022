import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day4Test extends AnyFreeSpec with Matchers {

  private val testInput = """2-4,6-8
                    |2-3,4-5
                    |5-7,7-9
                    |2-8,3-7
                    |6-6,4-6
                    |2-6,4-8""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must return the number of pairs that should be reconsidered" in {
      Day4.part1(testInput) mustEqual 2
    }

    "result" in {
      Day4.part1(input) mustEqual 602
    }
  }

  "part 2" - {

    "must return the number of pairs that overlap" in {
      Day4.part2(testInput) mustEqual 4
    }

    "result" in {
      Day4.part2(input) mustEqual 891
    }
  }
}
