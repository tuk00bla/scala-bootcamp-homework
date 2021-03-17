import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Action._

 class CalculatorSpec extends AnyFunSuite {
   test("suM    9 2   10  create Command Sum(9, 2, 10)") {
     parseCommand("sUm    9 2   10  ") == Right(Sum(List(9, 2, 10)))
   }
   test("   dIvIde 18,9 5 create Command Divide(18.9, 5)") {
      parseCommand("   DIvIde 18,9 5") == Right(Divide(List(18.9, 5)))
    }
   test("   Average 9 10  7,0 create Command Average(9, 10, 7)") {
      parseCommand("   AvErage 9 10  7,0 ") == Right(Average(List(9, 10, 7)))
    }
   test("min 2 -0 5 create Command Min(2,0,5)"){
      parseCommand("min 2 -0 5") == Right(Min(List(2, 0, 5)))
    }
   test("max -2 -10 100 create Command Max(-2, -10, 100)") {
      parseCommand("max -2 -10 100") == Right(Max(List(-2, -10, 100)))
    }
   test("max create ErrorMessage Error: no numbers"){
      parseCommand("max ") == Left(ErrorMessage("Error: no numbers"))
    }
    test("divide o d create ErrorMessage Error: invalid numbers") {
      parseCommand("divide 3 d") == Left(ErrorMessage("Error: invalid numbers"))
    }
    test("kek 3 3 create ErrorMessage Error: invalid command") {
      parseCommand("kek 3 3") == Left(ErrorMessage("Error: invalid command"))
    }
  }

  class CalculateSpec extends AnyFlatSpec with Matchers {
    "Divide by 0" should " shows Error: divide by 0" in {
      calculate(Divide(List(100, 0))) shouldEqual Left(ErrorMessage("Error: divide by 0"))
    }
    "Sum" should "returns SumResult" in {
      calculate(Sum(List(5, 7))) shouldEqual Right(SumResult(List(12, 5, 7)))
    }
    "Divide" should "returns DivideResult" in {
      calculate(Divide(List(36, 6))) shouldEqual Right(DivideResult(List(6, 36, 6)))
    }
    "Average" should "returns AverageResult" in {
      calculate(Average(List(3, 4, -1))) shouldEqual Right(AverageResult(List(2, 3, 4, -1)))
    }
    "Min" should "returns MinResult" in {
      calculate(Min(List(6, 0, 10, 1))) shouldEqual Right(MinResult(List(0, 6, 0, 10, 1)))
    }
    "Max" should "returns MaxResult" in {
      calculate(Max(List(13, 10, 22, 100))) shouldEqual Right(MaxResult(List(100, 13, 10, 22, 100)))
    }
    "Calculator" should "shows Error: invalid command" in {
      val otherCommand: Command = new Command {}
      calculate(otherCommand) shouldEqual Left(ErrorMessage("Error: invalid command"))
    }
  }
  class RenderSpec extends AnyFreeSpec {
    "rendering answer" - {
      renderResult(SumResult(List(9, 3, 4, 2, 1))) == Right("the sum of 9 3 4 2 1 is 19")
      renderResult(DivideResult(List(7, 49, 7))) == Right("49 divided by 7 is 7")
      renderResult(AverageResult(List(3, 5, 2, 2))) == Right("the average of 5 2 2 is 3")
      renderResult(MinResult(List(0, 0, 1))) == Right("the minimum of 0 1 is 0")
      renderResult(MaxResult(List(55, 100, 57, 99, 100))) == Right("the maximum of 55 100 57 99 100 is 100")
    }
    "returns error message: render error" - {
      val otherResult: Result = new Result {}
      renderResult(otherResult) == Left(ErrorMessage("Error: render error"))
    }
  }
