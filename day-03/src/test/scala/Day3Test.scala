import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day3Test extends AnyFreeSpec with Matchers {

  private val testInput = """vJrwpWtwJgWrhcsFMMfFFhFp
                            |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                            |PmmdzqPrVvPwwTWBwg
                            |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                            |ttgJtRGJQctTZtZT
                            |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must return the calculated error stats" in {
      Day3.part1(testInput) mustEqual 157
    }

    "return the result" in {
      Day3.part1(input) mustEqual 7845
    }
  }

  "part 2" - {

    "must return the sum of the badge values" in {
      Day3.part2(testInput) mustEqual 70
    }

    "return the result" in {
      Day3.part2(input) mustEqual 2790
    }
  }
}
