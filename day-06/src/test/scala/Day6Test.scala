import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day6Test extends AnyFreeSpec with Matchers {

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for test input" in {
      Day6.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb") mustEqual 7
      Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz") mustEqual 5
      Day6.part1("nppdvjthqldpwncqszvftbrmjlhg") mustEqual 6
      Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") mustEqual 10
      Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") mustEqual 11
    }

    "must work for puzzle input" in {
      Day6.part1(input) mustEqual 1892
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") mustEqual 19
      Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz") mustEqual 23
      Day6.part2("nppdvjthqldpwncqszvftbrmjlhg") mustEqual 23
      Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") mustEqual 29
      Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") mustEqual 26
    }

    "must work for the puzzle input" in {
      Day6.part2(input) mustEqual 2313
    }
  }
}
