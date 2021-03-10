import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.YearMonth
import scala.util.Try

object ErrorHandling {
  case class PaymentCard(holderName: String, number: String, expirationDate: YearMonth, securityCode: Int)

  sealed trait ValidationError

  object ValidationError {
    final case object InvalidCardNumber extends ValidationError {
      override def toString: String = "Card number is 16 digits printed on your card. Expected format: XXXX XXXX XXXX XXXX"
    }

    final case object InvalidExpirationDate extends ValidationError {
      override def toString: String = "Enter expiration date in format mm/yyyy"
    }

    final case object ExpiredCard extends ValidationError {
      override def toString: String = "Credit card is expired"
    }

    final case object HolderNameIsRequired extends ValidationError {
      override def toString: String = "Holder name is required"
    }

    final case object InvalidHolderName extends ValidationError {
      override def toString: String = "Holder name should contain only alphabetic characters."
    }
    final case object SecurityCodeIsInvalid extends ValidationError {
      override def toString: String = "Security code must consist of digits only"
    }

    final case object SecurityCodeLengthIsInvalid extends ValidationError {
      override def toString: String = "Security code must have a 3 digits length"
    }
  }
  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (
        validateHolderName(name),
        validateCardNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)).mapN(PaymentCard)
    }

    def validateHolderName(holderName: String): AllErrorsOr[String] = {
      if (holderName.isEmpty) {
        ValidationError.HolderNameIsRequired.invalidNec
      } else if (!holderName.matches("^(([A-Za-z]+ ?){1,3})$")) {
        ValidationError.InvalidHolderName.invalidNec
      } else {
        holderName.validNec
      }
    }

    def validateCardNumber(number: String): AllErrorsOr[String] = {
      if (number.matches("^\\d{4} \\d{4} \\d{4} \\d{4}$")) {
        number.validNec
      } else {
        ValidationError.InvalidCardNumber.invalidNec
      }
    }

    def validateExpirationDate(expDate: String): AllErrorsOr[YearMonth] = {
      val date = raw"(\d{2})\\(\d{2})".r
      expDate match {
        case date(month, year) =>
          Try(YearMonth.of(year.toInt, month.toInt)).toOption
            .map(dt => {
              if (dt.isAfter(YearMonth.now())) dt.validNec else ValidationError.ExpiredCard.invalidNec
            }).getOrElse(ValidationError.InvalidExpirationDate.invalidNec)
        case _ => ValidationError.InvalidExpirationDate.invalidNec
      }
    }

    def validateSecurityCode(securityCode: String): AllErrorsOr[Int] = {
      if (!securityCode.matches("^[0-9]+$")) {
          ValidationError.SecurityCodeIsInvalid.invalidNec
      } else if (securityCode.length != 3) {
          ValidationError.SecurityCodeLengthIsInvalid.invalidNec
      } else {
          securityCode.toInt.validNec
      }
    }

  }
}