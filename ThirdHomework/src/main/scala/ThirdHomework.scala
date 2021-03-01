import scala.io.Source
import ThirdHomework.Command._
import ThirdHomework.CommandValues._
import ThirdHomework.ErrorMessage._
object ThirdHomework {

  object CommandValues {
    val Divide = "divide"
    val Sum = "sum"
    val Average = "average"
    val Min = "min"
    val Max = "max"
    val Log = "log"
    val Exp = "exp"
  }

  sealed trait Command

  object Command {

    final case class DivideClass(dividend: Option[Double], divisor: Option[Double]) extends Command

    final case class SumClass(numbers: List[Option[Double]]) extends Command

    final case class AverageClass(numbers: List[Option[Double]]) extends Command

    final case class MinClass(numbers: List[Option[Double]]) extends Command

    final case class MaxClass(numbers: List[Option[Double]]) extends Command

    final case class LogClass(base: Option[Double], number: Option[Double]) extends Command

    final case class ExpClass(base: Option[Double], exponent: Option[Double]) extends Command

  }

  sealed trait ErrorMessage

  object ErrorMessage {

    final case object InvalidCommandError extends ErrorMessage

    final case object DivisionByZeroError extends ErrorMessage

    final case object CalculationError extends ErrorMessage

  }

  final case class RenderData(command: String, numbers: List[Option[Double]], calculatedValue: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    val parsedList = x.toLowerCase.split("\\s+").toList
    val dividedList = (parsedList.head, parsedList.tail.map(_.toDoubleOption))

    dividedList match {
      case (Divide, dividend :: divisor :: _) => Right(DivideClass(dividend, divisor))
      case (Sum, list@_ :: _ :: _) => Right(SumClass(list))
      case (Average, list@_ :: _ :: _) => Right(AverageClass(list))
      case (Min, list@_ :: _ :: _) => Right(MinClass(list))
      case (Max, list@_ :: _ :: _) => Right(MaxClass(list))
      case (Log, base :: number :: _) => Right(LogClass(base, number))
      case (Exp, base :: exponent :: _) => Right(ExpClass(base, exponent))
      case _ => Left(ErrorMessage.InvalidCommandError)
    }
  }

  def calculate(x: Command): Either[ErrorMessage, RenderData] = {

    def exponentiate(base : Double, exponent : Double) : Double =
      if (exponent == 0)
        1.0
      else
        base * exponentiate(base, exponent - 1)

    x match {
      case DivideClass(_, Some(0)) =>
        Left(ErrorMessage.DivisionByZeroError)
      case DivideClass(Some(dividend), Some(divisor)) =>
        Right(RenderData(Divide, List(Some(dividend), Some(divisor)), dividend / divisor))
      case SumClass(list) if list.forall(_.isDefined) =>
        Right(RenderData(Sum, list, list.map(_.get).sum))
      case AverageClass(list) if list.forall(_.isDefined) =>
        Right(RenderData(Average, list, list.map(_.get).sum / list.length))
      case MinClass(list) if list.forall(_.isDefined) =>
        Right(RenderData(Min, list, list.map(_.get).min))
      case MaxClass(list) if list.forall(_.isDefined) =>
        Right(RenderData(Max, list, list.map(_.get).max))
      case LogClass(Some(base), Some(number)) =>
        Right(RenderData(Log, List(Some(base), Some(number)), scala.math.log10(number) / scala.math.log10(base)))
      case ExpClass(Some(base), Some(exponent)) =>
        Right(RenderData(Exp, List(Some(base), Some(exponent)), exponentiate(base, exponent)))
      case _ => Left(ErrorMessage.InvalidCommandError)
    }
  }

  def renderResult(result: RenderData): String = {
    result.command match {
      case Divide => s"${result.numbers.head} divided by ${result.numbers.tail.head} is ${result.calculatedValue}"
      case Exp => s"Exp of ${result.numbers.head} and base ${result.numbers.tail.head} is ${result.calculatedValue}"
      case Log => s"Log of ${result.numbers.head} and base ${result.numbers.tail.head} is ${result.calculatedValue}"
      case _ => s"the ${result.command} of ${result.numbers.map(_.get).mkString(" ")} is ${result.calculatedValue}"
    }
  }

  def renderError(error: ErrorMessage): String = {
    val strError = error match {
      case ErrorMessage.InvalidCommandError => "Invalid command"
      case ErrorMessage.DivisionByZeroError => "Can't divide by zero"
      case ErrorMessage.CalculationError => "Calculation error"
    }
    s"Error: $strError"
  }

  def process(enterLine: String): String = {
    val processResult = for {
      command <- parseCommand(enterLine)
      result <- calculate(command)
    } yield result

    processResult.fold(renderError, renderResult)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}


