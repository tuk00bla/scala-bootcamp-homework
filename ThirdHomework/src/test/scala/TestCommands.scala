import ThirdHomework.Command._
import ThirdHomework.{ErrorMessage, RenderData, renderResult}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper




class CommandParsingSpec extends AnyFunSuite {
  test("Command parsing should catch empty input"){
    val output = ThirdHomework.parseCommand("")
    val expectedOutput = Left(ErrorMessage.InvalidCommandError)
    assert(output === expectedOutput)
  }
  test("Division by zero"){
    val output = ThirdHomework.parseCommand("divide 1.0 0")
    val expectedOutput = Right(DivideClass(Some(1.0), Some(0.0)))
    assert(output === expectedOutput)
  }
  test("Calculate result successfully"){
    val output = ThirdHomework.parseCommand("sum 2.0 5.9")
    val expectedOutput =  Right(SumClass(List(Some(2.0), Some(5.9))))
    assert(output === expectedOutput)
  }
  test("Log result successfully"){
    val output = ThirdHomework.parseCommand("log 4.0 20.0")
    val expectedOutput =  Right(LogClass(Some(4.0), Some(20.0)))
    assert(output === expectedOutput)
  }
  test("Exp result successfully"){
    val output = ThirdHomework.parseCommand("exp 2.0 4.0")
    val expectedOutput =  Right(ExpClass(Some(2.0), Some(4.0)))
    assert(output === expectedOutput)
  }
  test("Render result successfully"){
    val output = ThirdHomework.parseCommand("sum 2.0 5.9")
    val expectedOutput =  Right(SumClass(List(Some(2.0), Some(5.9))))
    assert(output === expectedOutput)
  }
}
