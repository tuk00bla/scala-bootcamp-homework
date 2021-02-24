import scala.io.Source
import ThirdHomework.Command._
import ThirdHomework.CommandValues._
import ThirdHomework.ErrorMessage._

object ThirdHomework {

  object CommandValues {
    val DIVIDE = "divide"
    val SUM = "sum"
    val AVERAGE = "average"
    val MIN = "min"
    val MAX = "max"
    val LOG = "log"
    val EXP = "exp"
  }

  sealed trait Command

  object Command {

    final case class Divide(dividend: Option[Double], divisor: Option[Double]) extends Command

    final case class Sum(numbers: List[Option[Double]]) extends Command

    final case class Average(numbers: List[Option[Double]]) extends Command

    final case class Min(numbers: List[Option[Double]]) extends Command

    final case class Max(numbers: List[Option[Double]]) extends Command

    final case class Log(numbers: List[Option[Double]]) extends Command

    final case class Exp(base: Option[Double], exponent: Option[Double]) extends Command

  }

  sealed trait Error {
    def value: String
  }

  object ErrorMessage {

    final case class InvalidCommandError(value: String) extends Error

    final case class DivisionByZeroError(value: String) extends Error

    final case class CalculationError(value: String) extends Error

  }

  final case class RenderData(command: String, numbers: List[Option[Double]], calculatedValue: Double)

  def parseCommand(x: String): Either[Error, Command] = {

    val parsedList = x.toLowerCase.split("\\s+").toList
    val dividedList = (parsedList.head, parsedList.tail.map(_.toDoubleOption))

    dividedList match {
      case (DIVIDE, dividend :: divisor :: _) => Right(Divide(dividend, divisor))
      case (SUM, list@_ :: _ :: _) => Right(Sum(list))
      case (AVERAGE, list@_ :: _ :: _) => Right(Average(list))
      case (MIN, list@_ :: _ :: _) => Right(Min(list))
      case (MAX, list@_ :: _ :: _) => Right(Max(list))
      case (LOG, list@_ :: _ :: _) => Right(Log(list))
      case (EXP, base :: exponent :: _) => Right(Exp(base, exponent))
      case _ => Left(InvalidCommandError("Command not found"))
    }
  }

  def calculate(x: Command): Either[Error, RenderData] = {

    def exponentiate(base : Double, exponent : Double) : Double =
      if (exponent == 0)
        1.0
      else
        base * exponentiate(base, exponent - 1)

    x match {
      case Divide(_, Some(0)) =>
        Left(DivisionByZeroError("Division by zero"))
      case Divide(Some(dividend), Some(divisor)) =>
        Right(RenderData(DIVIDE, List(Some(dividend), Some(divisor)), dividend / divisor))
      case Sum(list) if list.forall(_.isDefined) =>
        Right(RenderData(SUM, list, list.map(_.get).sum))
      case Average(list) if list.forall(_.isDefined) =>
        Right(RenderData(AVERAGE, list, list.map(_.get).sum / list.length))
      case Min(list) if list.forall(_.isDefined) =>
        Right(RenderData(MIN, list, list.map(_.get).min))
      case Max(list) if list.forall(_.isDefined) =>
        Right(RenderData(MAX, list, list.map(_.get).max))
      case Log(list) if list.forall(_.isDefined) =>
        Right(RenderData(LOG, list, list.map(_.get).))
      case Exp(Some(base), Some(exponent)) =>
        Right(RenderData(EXP, List(Some(base), Some(exponent)), exponentiate(base, exponent)))
      case _ => Left(CalculationError("Can not perform calculation on provided input"))
    }
  }

  def renderResult(x: RenderData): String = {
    x.command match {
      case DIVIDE => s"${x.numbers.head} divided by ${x.numbers.tail.head} is ${x.calculatedValue}"
      case EXP => s"${x.numbers.head} is by ${x.numbers.tail.head} is ${x.calculatedValue}"
      case _ => s"the ${x.command} of ${x.numbers.map(_.get).mkString(" ")} is ${x.calculatedValue}"
    }
  }

  def process(x: String): String = {
    (for {
      y <- parseCommand(x)
      z <- calculate(y)
    } yield renderResult(z)) match {
      case Right(value) => value
      case Left(error) => error.value
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}


