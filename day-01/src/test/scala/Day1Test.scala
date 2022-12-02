import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day1Test extends AnyFreeSpec with Matchers {

  "part 1" - {

    "must return how many scans increase from the previous value" in {

      val input = List(
        199,
        200,
        208,
        210,
        200,
        207,
        240,
        269,
        260,
        263
      )

      Day1.part1(input) mustEqual 7
    }

    "must work for the puzzle input" in {

      val input = Source.fromResource("input.txt").getLines().map(_.toInt).toList
      Day1.part1(input) mustEqual 1581
    }
  }

  "part 2" - {

    "must return how many sliding sums increase from the previous" in {

      val input = List(
        199,
        200,
        208,
        210,
        200,
        207,
        240,
        269,
        260,
        263
      )

      Day1.part2(input) mustEqual 5
    }

    "must work for the puzzle input" in {

      val input = Source.fromResource("input.txt").getLines().map(_.toInt).toList
      Day1.part2(input) mustEqual 1618
    }
  }
}
