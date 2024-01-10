package blogs

import blogs.TimeTravellingWithCE.RetryingService.{BetweenRetry, MaxRetries}
import cats.effect.{Clock, ContextShift, IO, Timer}
import cats.implicits.*

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, TimeoutException}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// https://blog.softwaremill.com/time-traveling-in-tests-with-cats-effect-b22084f6a89
object TimeTravellingWithCE:
  // we want to test some error scenarios which incorporate timeouts, restarts, and similar actions.
  // Sometimes we may need higher-level integration tests which verify such supervision logic,
  // and such tests need to provide a Timer[F].
  trait Service:
    def run: IO[Unit]

  class TimeoutService(underlying: Service)(using ec: ExecutionContext, timer: Timer[IO]) extends Service:
    implicit val cs: ContextShift[IO] = IO.contextShift(ec)
    def run: IO[Unit] =
      IO.race(
        timer.sleep(TimeoutService.DefaultTimeout),
        IO(println("Call underlying service...")) *> underlying.run
      ).flatMap {
        case Right(resp) => IO.pure(resp)
        case Left(_)     => IO.raiseError(new TimeoutException("Service call timed out"))
      }

  object TimeoutService:
    val DefaultTimeout: FiniteDuration = 3000.millis

  class RetryingService(underlying: Service)(using timer: Timer[IO]) extends Service:
    def run: IO[Unit] = retry(MaxRetries, underlying.run, error = None)

    private def retry(retries: Int, body: => IO[Unit], error: Option[Throwable]): IO[Unit] =
      if retries == 0 then error.map(IO.raiseError).getOrElse(IO.raiseError(new MaxRetriesException))
      else
        IO(println("Call underlying service...")) *>
          body.attempt.flatMap {
            case Left(error)  => timer.sleep(BetweenRetry) *> retry(retries - 1, body, Option(error))
            case Right(value) => IO.pure(value)
          }

  object RetryingService:
    val BetweenRetry: FiniteDuration = 1000.millis
    val MaxRetries                   = 5

  final case class MaxRetriesException(msg: String = s"Max retries $MaxRetries reached") extends Exception(msg)
