import ThirdHomework.Command._
import ThirdHomework.ErrorMessage._
import org.scalatest.funsuite.AnyFunSuite


class CommandParsingSpec extends AnyFunSuite {
  test("Command parsing should catch empty input"){
    val output = ThirdHomework.parseCommand("")
    val expectedOutput = Left(InvalidCommandError("Command not found"))
    assert(output === expectedOutput)
  }
  test("division by zero"){
    val output = ThirdHomework.parseCommand("divide 1.0 0")
    val expectedOutput = Right(Divide(Some(1.0), Some(0.0)))
    assert(output === expectedOutput)
  }
  test("Calculate result successfully"){
    // "Command parsing" should "tolerate empty input" in {
    val output = ThirdHomework.parseCommand("sum 2.0 5.9")
    val expectedOutput =  Right(Sum(List(Some(2.0), Some(5.9))))
    assert(output === expectedOutput)
  }
  test("Render result successfully"){
    // "Command parsing" should "tolerate empty input" in {
    val output = ThirdHomework.parseCommand("sum 2.0 5.9")
    val expectedOutput =  Right(Sum(List(Some(2.0), Some(5.9))))
    assert(output === expectedOutput)
  }
}