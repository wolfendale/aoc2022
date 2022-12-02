import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day1Test extends AnyFreeSpec with Matchers {

  "part 1" - {

    "must return the calories carried by the elf with the most calories" in {

      val input = """1000
                    |2000
                    |3000
                    |
                    |4000
                    |
                    |5000
                    |6000
                    |
                    |7000
                    |8000
                    |9000
                    |
                    |10000""".stripMargin

      Day1.mostCalories(input) mustEqual 24000
    }

    "result" in {

      val input = Source.fromResource("input.txt").mkString

      Day1.mostCalories(input) mustEqual 72511
    }
  }

  "part 2" - {

    "must return the sum of the calories carried by the 3 elves with the most calories" in {

      val input = """1000
                    |2000
                    |3000
                    |
                    |4000
                    |
                    |5000
                    |6000
                    |
                    |7000
                    |8000
                    |9000
                    |
                    |10000""".stripMargin

      Day1.sumOfTop3MostCalories(input) mustEqual 45000
    }

    "result" in {

      val input = Source.fromResource("input.txt").mkString

      Day1.sumOfTop3MostCalories(input) mustEqual 0
    }
  }
}
