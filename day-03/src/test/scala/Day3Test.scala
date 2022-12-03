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

  "day 1" - {

    "must return the calculated error stats" in {
      Day3.part1(testInput) mustEqual 157
    }

    "return the result" in {
      Day3.part1(input) mustEqual 7845
    }
  }
}
