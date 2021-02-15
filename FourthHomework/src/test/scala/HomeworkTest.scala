import FourthHomework._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HomeworkTest extends AnyFunSuite {
  test("Correct sum of array"){
    val arr = Array (1,2,3,4)
    val output = FourthHomework.runningSumOfArray(arr)
    val expectedOutput = Array(1,3,6,10)
    assert(output === expectedOutput)
  }
  test("Correct kidsWithCandies"){
    val cand = Array (2,3,5,1,3)
    val extraCand = 3
    val output = FourthHomework.kidsWithCandies(cand, extraCand)
    val expectedOutput = Array(true,true,true,false,true)
    assert(output === expectedOutput)
  }
}