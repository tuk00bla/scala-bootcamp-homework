import io.circe.Json
import cats.effect.{Blocker, ExitCode, IO, IOApp, Sync}
import cats._
import cats.syntax.all._
import scala.annotation.tailrec
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import scala.util.Try

object Router {
  type RouteFind[F[+_], +A, +E] = Request[F] => Option[RouteAction[F, A, E]]
  type RouteExecute[F[+_], +A, +E] = Request[F] => F[Either[E, A]]

  trait RouteAction[F[+_], +A, +E] {
    def execute: RouteExecute[F, A, E]
  }

  trait Route[F[+_], +A, +E] {
    def check: RouteFind[F, A, E]
  }

  trait Router[F[+_], +A, +E] {
    def find(req: Request[F]): Option[RouteAction[F, A, E]]
  }

  trait RouteRoot {
    val path: Path
  }

  final class GameRouter[F[+_], +A, +E] private (val routes: List[Route[F, A, E]]) extends Router[F, A, E] {
    override def find(req: Request[F]): Option[RouteAction[F, A, E]] = {
      @tailrec
      def recursive(routes: List[Route[F, A, E]]): Option[RouteAction[F, A, E]] = {
        routes match {
          case route :: tail =>
            Try(route.check(req)).toOption.flatten match {
              case act @ Some(_) => act
              case _ => recursive(tail)
            }
          case _ => None
        }
      }

      recursive(routes)
    }
  }

  final class GameRoute[F[+_], +A, +E](checkF: RouteFind[F, A, E]) extends Route[F, A, E] {
    override def check: RouteFind[F, A, E] = checkF
  }

  object GameRoute {
    def apply[F[+_], A, E](checkF: RouteFind[F, A, E]): GameRoute[F, A, E]
    = new GameRoute(checkF)
  }

  object GameRouter {
    def ofRoutes[F[+_], A, E](routes: List[Route[F, A, E]]): Router[F, A, E] = new GameRouter[F, A, E](routes)
  }

  trait RouteResponse {

  }

  case class GameRouteResponse(resp: String = "") extends RouteResponse

  object GameRouteResponse {
    def apply(value: String): GameRouteResponse = new GameRouteResponse(value)
  }

  sealed case class GameRouteRoot(path: Path) extends RouteRoot
}

object Games {
  import Router._

  trait GameError extends Error
  object GameErrors {
    case object WrongParams extends GameError
    case object TokenNotFound extends GameError
    case object InvalidToken extends GameError
    case object NumberMinMoreMaxError extends GameError
    case object NumberMinEqualsMaxError extends GameError
    case object ValueLessThanResult extends GameError
    case object ValueMoreThanResult extends GameError
    case object GameNotStarted extends GameError
  }

  trait Game {}

  trait GameActionResponse[F[_]] {}

  implicit val responseEnc: EntityEncoder[IO, GameActionResponse[IO]] = new EntityEncoder[IO, GameActionResponse[IO]] {
    override def toEntity(a: GameActionResponse[IO]): Entity[IO] = Entity.empty

    override def headers: Headers = Headers.empty
  }

  trait GameRoutes {
    def routesOf[F[+_] : Applicative](root: Path): List[Route[F, GameActionResponse[F], GameError]]
  }

  final case class TheGuessStartParams(min: Long, max: Long) {}
  final case class TheGuessStartResult[F[_]](min: Long, max: Long, result: Long) extends GameActionResponse[F] {}

  final case class TheGuessPickParams(num: Long) {}
  final case class TheGuessPickResult[F[_]](result: Long) extends GameActionResponse[F] {}

  final class TheGuess private extends Game {
    def start[F[_] : Applicative](params: TheGuessStartParams): Either[GameError, TheGuessStartResult[F]] = {
      params match {
        case TheGuessStartParams(min, max) if min > max => Left(GameErrors.NumberMinMoreMaxError)
        case TheGuessStartParams(min, max) if min == max => Left(GameErrors.NumberMinEqualsMaxError)
        case TheGuessStartParams(min, max) => Right(TheGuessStartResult(min ,max, getRandomNumber(min, max)))
        case _ => Left(GameErrors.WrongParams)
      }
    }

    def pick[F[_]: Applicative](params: TheGuessPickParams, result: Long): Either[GameError, TheGuessPickResult[F]] = {
      if (params.num > result) Left(GameErrors.ValueMoreThanResult)
      else if (params.num < result) Left(GameErrors.ValueLessThanResult)
      else Right(TheGuessPickResult[F](result))
    }

    def getRandomNumber(min: Long, max: Long): Long = scala.util.Random.between(min, max)
  }

  object TheGuess {
    def routesOf[F[+_] : Applicative](root: RouteRoot)(implicit mt: MonadThrow[F], s: Sync[F]): List[GameRoute[F, GameActionResponse[F], GameError]] = {
      GameRoute[F, GameActionResponse[F], GameError](
        {
          case POST -> root.path / "start" => Some(new TheGuessController.Start[F])
        },
      ) ::
        GameRoute[F, GameActionResponse[F], GameError](
          {
            case POST -> root.path / "pick"  => Some(new TheGuessController.Pick[F])
          },
        ) :: Nil
    }

    def apply: TheGuess = new TheGuess

    object TheGuessController {

      import io.circe.generic.auto._
      import org.http4s.circe.CirceEntityCodec._

      class Start[F[+_]](implicit mt: MonadThrow[F], s: Sync[F]) extends RouteAction[F, TheGuessStartResult[F], GameError] {
        override def execute: RouteExecute[F, TheGuessStartResult[F], GameError] = (req: Request[F]) => {
          req.as[TheGuessStartParams].flatMap { params =>
            Sync[F].delay(apply.start[F](params))
          }
        }
      }

      class Pick[F[+_]](implicit mt: MonadThrow[F], s: Sync[F]) extends RouteAction[F, TheGuessPickResult[F], GameError] {
        override def execute: RouteExecute[F, TheGuessPickResult[F], GameError] = (req: Request[F]) => {
          req.as[TheGuessPickParams].flatMap { params =>
            Sync[F].pure(params).map { params =>
              req.cookies
                .find(_.name == "guess-res")
                .flatMap(_.content.toLongOption)
                .toRight(GameErrors.GameNotStarted)
                .flatMap { gameResult =>
                  apply.pick[F](params, gameResult)
                }
            }
          }
        }
      }

    }
  }

}

object Client extends IOApp {

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9876/api/v1/the-guess"
  private val min = 0
  private val max = 100

  case class GuessStartRequest(min: Long, max: Long)
  case class GuessPickRequest(num: Long)
  case class GuessResponse(result: String, message: String)

  private def pick(client: Client[IO], cookie: ResponseCookie, num: Long): IO[GuessResponse] = {
    def recursive(num: Long): IO[GuessResponse] = {
      client.expect[GuessResponse](
        Method.POST(GuessPickRequest(num), uri / "pick").map(_.addCookie(cookie.name, cookie.content))
      ).handleErrorWith(_ => {
        recursive(num + 1)
      })
    }

    recursive(num)
  }

  private def start(client: Client[IO], min: Long, max: Long): IO[Either[Throwable, ResponseCookie]] = {
    Method.POST(
      GuessStartRequest(min, max),
      uri / "start"
    ).flatMap[Either[Throwable, ResponseCookie]](client.run(_).use { resp: Response[IO] =>
      if (resp.status == Status.Ok) {
        IO.pure(resp.cookies.find(_.name == "guess-result").toRight(new Error("Not found")))
      } else {
        resp.bodyText
          .map(e => {
            Left(new Error(e))
          }).compile.lastOrError
      }
    })
  }

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global)
      .resource
      .parZip(Blocker[IO]).use { case (client, _) =>
      for {
        startResult <- start(client, min ,max)
        cookie <- IO.fromEither(startResult)
        pickResponse <- pick(client, cookie, min)
        _ <- IO.pure(println(pickResponse.message))
      } yield ()
    }.as(ExitCode.Success)
  }
}

object Server extends IOApp {
  import Router._
  import Games._

  import org.http4s.circe.CirceEntityCodec._

  private val ApiRoot = Root / "api" / "v1"
  private val GuessRoot = GameRouteRoot(ApiRoot / "the-guess")

  object Formatter {
    private def errorFromString(e: String): Json = {
      Json.obj(
        "result" -> Json.fromString("error"),
        "message" -> Json.fromString(e)
      )
    }

    def formatGameError(e: GameError): IO[Response[IO]] = e match {
      case GameErrors.WrongParams => BadRequest(errorFromString("Wrong incoming params"))
      case GameErrors.NumberMinEqualsMaxError => BadRequest(errorFromString("Min value can't be equals max value"))
      case GameErrors.NumberMinMoreMaxError => BadRequest(errorFromString("Min value can't be more then max value"))
      case GameErrors.TokenNotFound => BadRequest(errorFromString("Please provide access token"))
      case GameErrors.InvalidToken => BadRequest(errorFromString("Token is invalid"))
      case GameErrors.ValueLessThanResult => BadRequest(errorFromString("Value less than result"))
      case GameErrors.ValueMoreThanResult => BadRequest(errorFromString("Value more than result"))
      case GameErrors.GameNotStarted => BadRequest(errorFromString("Game not started"))
      case _ => InternalServerError(errorFromString("Unsupported error type"))
    }

    def formatGameResult(r: GameActionResponse[IO]): IO[Response[IO]] = r match {
      case TheGuessStartResult(min, max, result) => Ok(
        Json.obj(
          "result" -> Json.fromString("OK"),
          "message" -> Json.fromString(s"Game started with params $min - $max")
        )
      ).map(_.addCookie("guess-result", result.toString))
      case TheGuessPickResult(res) => Ok(Json.obj(
        "result" -> Json.fromString("OK"),
        "message" -> Json.fromString(s"Game is ended, result = $res.")
      )).map(_.removeCookie("guess-result"))
      case _ => Ok(Json.Null)
    }
  }

  private val gamesRoutes = {
    val router = GameRouter.ofRoutes[IO, GameActionResponse[IO], GameError](
      TheGuess.routesOf[IO](GuessRoot)
    )

    HttpRoutes.of[IO] { req =>
      (for {
        route <- router.find(req)
        result <- Some(route.execute(req))
      } yield result) match {
        case Some(result) => result flatMap {
          case Left(e) => Formatter.formatGameError(e)
          case Right(r) => Formatter.formatGameResult(r)
        }
      }

    }
  }

  private val httpApp = gamesRoutes.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9876, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}