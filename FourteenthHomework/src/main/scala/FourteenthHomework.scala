import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util._

object FourteenthHomework {

  final class IO[A](run: () => A) {

    def map[B](f: A => B): IO[B] = IO(f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(run()).run)

    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())

    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)

    def option: IO[Option[A]] = redeem(_ => None, Some(_))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = Try(run()).toEither.fold(f, IO.pure)

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =  IO(Try(run()).toEither.fold(recover, map))

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = Try(run()).toEither.fold(recover, bind)

    def unsafeRunSync(): A = run()

    def unsafeToFuture(): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)

    def suspend[A](thunk: => IO[A]): IO[A] = thunk

    def delay[A](body: => A): IO[A] = new IO(() => body)

    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(a) => pure(a)
      case Left(err) => raiseError(err)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None => raiseError(orElse)
      case Some(value) => pure(value)
    }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(a) => pure(a)
      case Failure(err) => raiseError(err)
    }

    def none[A]: IO[Option[A]] = pure(None)

    def raiseError[A](e: Throwable): IO[A] = pure(throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = IO.unlessA(cond)(IO.raiseError(e))

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = IO.whenA(cond)(IO.raiseError(e))

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit

    val unit: IO[Unit] = pure(())
  }
}
