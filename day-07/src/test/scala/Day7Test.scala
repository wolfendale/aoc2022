import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day7Test extends AnyFreeSpec with Matchers {

  private val testInput = """$ cd /
                            |$ ls
                            |dir a
                            |14848514 b.txt
                            |8504156 c.dat
                            |dir d
                            |$ cd a
                            |$ ls
                            |dir e
                            |29116 f
                            |2557 g
                            |62596 h.lst
                            |$ cd e
                            |$ ls
                            |584 i
                            |$ cd ..
                            |$ cd ..
                            |$ cd d
                            |$ ls
                            |4060174 j
                            |8033020 d.log
                            |5626152 d.ext
                            |7214296 k""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for test input" in {
      Day7.part1(testInput) mustEqual 95437
    }

    "must work for puzzle input" in {
      Day7.part1(puzzleInput) mustEqual 1770595
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day7.part2(testInput) mustEqual 24933642
    }

    "must work for the puzzle input" in {
      Day7.part2(puzzleInput) mustEqual 2195372
    }
  }
}
