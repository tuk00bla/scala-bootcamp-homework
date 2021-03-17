import scala.io.Source

trait Command

  final case class Divide(numbers: List[Double])  extends Command
  final case class Sum(numbers: List[Double])     extends Command
  final case class Average(numbers: List[Double]) extends Command
  final case class Min(numbers: List[Double])     extends Command
  final case class Max(numbers: List[Double])     extends Command


final case class ErrorMessage(value: String)

trait Result

  final case class DivideResult(list: List[Double])  extends Result
  final case class SumResult(list: List[Double])     extends Result
  final case class AverageResult(list: List[Double]) extends Result
  final case class MinResult(list: List[Double])     extends Result
  final case class MaxResult(list: List[Double])     extends Result

object Action {

  def parseCommand(inputString: String): Either[ErrorMessage, Command] = {
    inputString.toLowerCase.trim
      .replace(',', '.')
      .split("\\s+")
      .toList match {
      case _ :: Nil => Left(ErrorMessage("Error: no numbers"))
      case _ :: numbers if numbers.map(s => s.toDoubleOption).contains(None) =>
        Left(ErrorMessage("Error: invalid numbers"))
      case "divide" :: numbers  => Right(Divide(numbers.map(_.toDouble)))
      case "sum" :: numbers     => Right(Sum(numbers.map(_.toDouble)))
      case "average" :: numbers => Right(Average(numbers.map(_.toDouble)))
      case "min" :: numbers     => Right(Min(numbers.map(_.toDouble)))
      case "max" :: numbers     => Right(Max(numbers.map(_.toDouble)))
      case _                    => Left(ErrorMessage("Error: invalid command"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = command match {
    case Divide(numbers) =>
      if (numbers(1) == 0) Left(ErrorMessage("Error: divide by 0"))
      else Right(DivideResult(List(numbers.head / numbers(1), numbers.head, numbers(1))))
    case Sum(numbers) => Right(SumResult(List(numbers.sum) ++ numbers))
    case Average(numbers) => Right(AverageResult(List(numbers.sum / numbers.length) ++ numbers))
    case Min(numbers) => Right(MinResult(List(numbers.min) ++ numbers))
    case Max(numbers) => Right(MaxResult(List(numbers.max) ++ numbers))
    case _ => Left(ErrorMessage("Error: invalid command"))
  }

  def renderResult(result: Result): Either[ErrorMessage, String] = result match {
    case DivideResult(list) => Right(f"${list(1)} divided by ${list(2)} is ${list.head}")
    case SumResult(list) => Right(f"the sum of ${list.tail.mkString(" ")} is ${list.head}")
    case AverageResult(list) => Right(f"the average of ${list.tail.mkString(" ")} is ${list.head}")
    case MinResult(list) => Right(f"the minimum of ${list.tail.mkString(" ")} is ${list.head}")
    case MaxResult(list) => Right(f"the maximum of ${list.tail.mkString(" ")} is ${list.head}")
    case _ => Left(ErrorMessage("Error: render error"))
  }
}
object Calculator {
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  def process(inputString: String): String = {
    val result: Either[ErrorMessage, String] = for {
      command <- Action.parseCommand(inputString)
      result  <- Action.calculate(command)
      render  <- Action.renderResult(result)
    } yield render

    result match {
      case Left(error)  => error.value
      case Right(value) => value
    }
  }
}