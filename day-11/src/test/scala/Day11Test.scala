import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day11Test extends AnyFreeSpec with Matchers {

  private val testInput = """Monkey 0:
                            |  Starting items: 79, 98
                            |  Operation: new = old * 19
                            |  Test: divisible by 23
                            |    If true: throw to monkey 2
                            |    If false: throw to monkey 3
                            |
                            |Monkey 1:
                            |  Starting items: 54, 65, 75, 74
                            |  Operation: new = old + 6
                            |  Test: divisible by 19
                            |    If true: throw to monkey 2
                            |    If false: throw to monkey 0
                            |
                            |Monkey 2:
                            |  Starting items: 79, 60, 97
                            |  Operation: new = old * old
                            |  Test: divisible by 13
                            |    If true: throw to monkey 1
                            |    If false: throw to monkey 3
                            |
                            |Monkey 3:
                            |  Starting items: 74
                            |  Operation: new = old + 3
                            |  Test: divisible by 17
                            |    If true: throw to monkey 0
                            |    If false: throw to monkey 1""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day11.part1(testInput) mustEqual 10605
    }

    "must work for the puzzle input" in {
      Day11.part1(puzzleInput) mustEqual 56120
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day11.part2(testInput) mustEqual 2713310158L
    }

    "must work for the puzzle input" in {
      Day11.part2(puzzleInput) mustEqual 24389045529L
    }
  }
}
