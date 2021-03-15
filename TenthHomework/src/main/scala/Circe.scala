import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter

import io.circe
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.parser._
import io.circe.generic.JsonCodec

import scalaj.http.Http

import scala.io.Source
import scala.util.Try

object Circe {

  @JsonCodec final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)

  @JsonCodec final case class TeamBox(totals: TeamTotals)

  object TeamBox {

    import io.circe.generic.semiauto._

    val decoderDerived: Decoder[TeamTotals] = deriveDecoder[TeamTotals]
    val decoderCamelSnake: Decoder[TeamTotals] = (c: HCursor) =>
      for {
        assists <- c.downField("assists").as[String]
        fullTimeoutRemaining <- c.downField("full_timeout_remaining").as[String]
        plusMinus <- c.downField("plusMinus").as[String]
      } yield {
        TeamTotals(assists, fullTimeoutRemaining, plusMinus)
      }
    implicit lazy val decoder: Decoder[TeamTotals] = decoderDerived or decoderCamelSnake
  }

  @JsonCodec final case class GameStats(hTeam: TeamBox, vTeam: TeamBox)

  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)

  object PrevMatchup {

    import io.circe.generic.semiauto._
    import cats.syntax.either._

    val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    implicit val dateEncoder: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.format(formatter))
    implicit val dateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap[LocalDate](str => {
      Either.catchNonFatal(LocalDate.parse(str, formatter)).leftMap(_.getMessage)
    })
    implicit val AEncoder: Encoder.AsObject[PrevMatchup] = deriveEncoder[PrevMatchup]
    implicit val ADecoder: Decoder[PrevMatchup] = deriveDecoder[PrevMatchup]
  }

  @JsonCodec final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )

  @JsonCodec final case class JustScore(score: String)

  @JsonCodec final case class TeamStats(
                                         linescore: List[JustScore],
                                         loss: String,
                                         score: String,
                                         teamId: String,
                                         triCode: String
                                       )

  @JsonCodec final case class GameDuration(hours: String, minutes: String)

  @JsonCodec final case class Arena(
                                     city: String,
                                     country: String,
                                     isDomestic: Boolean,
                                     name: String,
                                     stateAbbr: String
                                   )

  @JsonCodec final case class Game(
                                    arena: Arena,
                                    attendance: String,
                                    endTimeUTC: Option[ZonedDateTime],
                                    gameDuration: GameDuration,
                                    gameId: String,
                                    gameUrlCode: String,
                                    hTeam: TeamStats,
                                    isBuzzerBeater: Boolean,
                                    startTimeUTC: ZonedDateTime,
                                    vTeam: TeamStats,
                                  )

  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val jsonString = Try {
      val src = Source.fromResource(s"scoreboard_$dateString.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
      Http(url).asString.body
    }
    decode[Scoreboard](jsonString)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val jsonString = Try {
      val src = Source.fromResource(s"${gameId}_boxscore.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
      Http(url).asString.body
    }
    decode[BoxScore](jsonString)
  }
}