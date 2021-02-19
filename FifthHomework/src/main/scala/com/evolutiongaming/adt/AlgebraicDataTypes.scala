package com.evolutiongaming.adt

class AlgebraicDataTypes {

  sealed trait Rank

  object Rank {

    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank

    def convertToRank(value: String): Either[ErrorMessage, Rank] = {
      value match {
        case "2" => Right(Two)
        case "3" => Right(Three)
        case "4" => Right(Four)
        case "5" => Right(Five)
        case "6" => Right(Six)
        case "7" => Right(Seven)
        case "8" => Right(Eight)
        case "9" => Right(Nine)
        case "T" => Right(Ten)
        case "J" => Right(Jack)
        case "Q" => Right(Queen)
        case "K" => Right(King)
        case "A" => Right(Ace)
        case _ => Left(Error.IncorrectRank)
      }
    }
  }

  sealed trait Suit

  object Suit {

    final case object Clubs extends Suit

    final case object Diamonds extends Suit

    final case object Hearts extends Suit

    final case object Spades extends Suit

    def convertToSuit(value: String): Either[ErrorMessage, Suit] = {
      value match {
        case "c" => Right(Clubs)
        case "d" => Right(Diamonds)
        case "h" => Right(Hearts)
        case "s" => Right(Spades)
        case _ => Left(Error.IncorrectSuit)
      }
    }
  }

    final case class Card(rank: Rank, suit: Suit)

    sealed trait Hand

    object Hand {

      final case class TexasHoldem(cards: List[Card]) extends Hand {
      def apply(cards: List[Card]): Either[ErrorMessage, TexasHoldem] = {
        if (cards.length == 2) Right(TexasHoldem(cards))
        else Left(Error.IncorrectHandSize)
      }
    }

      final case class OmahaHoldem(cards: List[Card]) extends Hand {
        def apply(cards: List[Card]): Either[ErrorMessage, OmahaHoldem] = {
          if (cards.length == 4) Right(OmahaHoldem(cards))
          else Left(Error.IncorrectHandSize)
        }
      }
    }

    final case class Board(card: List[Card]) {
      def apply(cards: List[Card]): Either[ErrorMessage, Board] = {
        if (cards.length == 5) Right(Board(cards))
        else Left(Error.IncorrectBoardSize)
      }
    }

      sealed abstract class Combination

      object Combination {

        final case class StraightFlush(high: Rank) extends Combination
        final case class FourOfAKind(rank: Rank, rest: (Rank)) extends Combination
        final case class FullHouse(two: Rank, three: Rank) extends Combination
        final case class Flush(rank: Rank) extends Combination
        final case class Straight(high: Rank) extends Combination
        final case class ThreeOfAKind(rank: Rank, rest: (Rank, Rank))
          extends Combination
        final case class TwoPairs(low: Rank, high: Rank, rest: (Rank))
          extends Combination
        final case class Pair(rank: Rank, rest: (Rank, Rank, Rank))
          extends Combination
        final case class HighCard(rest: (Rank, Rank, Rank, Rank, Rank))
          extends Combination
      }

      abstract class Situation(board: Board, hands: List[Hand])

      object Situation {

        final case class TexasHoldem(board: Board, hands: List[Hand.TexasHoldem])
          extends Situation(board, hands)

        final case class OmahaHoldem(board: Board, hands: List[Hand.OmahaHoldem])
          extends Situation(board, hands)
      }

  sealed abstract class ErrorMessage(details: String) {
    def message: String = s"Error: $details"
  }

  object Error {

    case object IncorrectSuit extends ErrorMessage("Incorrect suit")

    case object IncorrectRank extends ErrorMessage("Incorrect rank")

    case object IncorrectBoardSize extends ErrorMessage("Invalid board size")

    case object IncorrectHandSize extends ErrorMessage("Invalid hand size")

  }

  case class TestCase(board: Board, hands: List[Hand])

  final case class TestResult(board: Board, hands: List[Hand])

}
