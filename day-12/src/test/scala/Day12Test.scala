import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day12Test extends AnyFreeSpec with Matchers {

  private val testInput = """Sabqponm
                            |abcryxxl
                            |accszExk
                            |acctuvwj
                            |abdefghi""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day12.part1(testInput) mustEqual 31
    }

    "must work for the puzzle input" in {
      Day12.part1(puzzleInput) mustEqual 484
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day12.part2(testInput) mustEqual 29
    }

    "must work for the puzzle input" in {
      Day12.part2(puzzleInput) mustEqual 478
    }
  }
}
