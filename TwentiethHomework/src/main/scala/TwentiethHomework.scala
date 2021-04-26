import GuessResult._
import cats.effect._
import cats.implicits._
import cats.kernel.Comparison.{EqualTo, GreaterThan, LessThan}
import fs2.concurrent.Queue
import fs2.{Chunk, Pipe, Pull, Stream}
import org.http4s._
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.util.Random


// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

sealed trait GuessResult {
  def value: String
}

object GuessResult {

  object Equals extends GuessResult {
    override val value: String = "equals"
  }
  object Less extends GuessResult {
    override val value: String = "less"
  }
  object Greater extends GuessResult {
    override val value: String = "greater"
  }
}

final case class GuessResponse(result: GuessResult)

object Utils {
  def printLine(string: String = ""): IO[Unit] = IO(println(string))
}

object GuessServer extends IOApp {

  private implicit val R: Random = Random

  private def routes(implicit R: Random) = HttpRoutes.of[IO] {
    case GET -> Root / "game" =>
      val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.pull.uncons1.flatMap {
          case Some((WebSocketFrame.Text(message, _), tail)) =>
            message.trim.split(",\\s*") match {
              case Array(from, to) => from.toLongOption.toValidNel("Invalid input: 'from' not a number")
                .product(to.toLongOption.toValidNel("Invalid input: 'to' not a number")).fold(
                errors => Pull.output(Chunk(WebSocketFrame.Text(errors.toString))),
                { case (from, to) =>
                  val toGuess = R.between(from, to)
                  (Stream(WebSocketFrame.Text("Value created. Try to guess")) ++ tail.map {
                    case WebSocketFrame.Text(message, _) =>
                      val rs = message.trim.toLongOption
                        .toValid("Invalid input: 'value' not a number").map { value =>
                        value.comparison(toGuess) match {
                          case GreaterThan => Greater
                          case EqualTo => Equals
                          case LessThan => Less
                        }
                      }.fold(identity, _.value)
                      WebSocketFrame.Text(rs)
                  }).pull.echo
                })

              case _ => Pull.output(Chunk(WebSocketFrame.Text("Invalid input: expected from,to")))
            }

          case None => Pull.done
        }.stream

      for {
        queue <- Queue.bounded[IO, WebSocketFrame](1024)
        response <- WebSocketBuilder[IO].build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = queue.enqueue,
          // Outgoing stream of WebSocket messages to send to the client.
          send = queue.dequeue.through(echoPipe),
        )
      } yield response
  }

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(routes.orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

object GuessClient extends IOApp {

  private val uri = uri"ws://localhost:8080"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri / "game")))

    clientResource.use { implicit client =>
      for {
        from <- IO(Random.nextLong(Long.MaxValue))
        to <- IO(Random.between(from, Long.MaxValue))
        _ <- client.send(WSFrame.Text(s"$from,$to"))
        _ <- client.receive.flatMap {
          case Some(WSFrame.Text(message, _)) =>
            printLine(message.trim) *> guessValue(from,to)

          case None => IO.raiseError(new Exception("No response from server"))
        }.map(_.toString) >>= printLine
      } yield ExitCode.Success
    }
  }

  def guessValue(from: Long, to: Long)(implicit client: WSConnectionHighLevel[IO]): IO[Long] =
    for {
      _ <- printLine(s"$from - $to")
      value <- IO(from + ((to - from) / 2))
      _ <- printLine(s"Sending: $value")
      _ <- client.send(WSFrame.Text(value.toString))
      rsIo <- client.receive
      rs = rsIo match {
        case Some(WSFrame.Text(message, _)) => message.trim
      }
      _ <- printLine(s"$value: $rs")
      res <- rs match {
        case Greater.value => guessValue(from, value)
        case Less.value => guessValue(to min value + 1, to)
        case Equals.value => IO.pure(value)
        case _ => IO.raiseError(new Exception("Invalid response when guessing number"))
      }
    } yield res
}