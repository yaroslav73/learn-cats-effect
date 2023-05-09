package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.implicits.*
import cats.syntax.*
import cats.syntax.all.*
import courses.rockthejvm.utils.general.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal - time-blocking effects
  trait _Temporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  // Abilities: pure, map/flatMap, raiseError, uncancelable, start, ref/deferred + sleep
  val temporalIO        = Temporal[IO] // Given Temporal[IO] in scope
  val chainOfEffectsOne = IO("Loading...").trace *> IO.sleep(1.second) *> IO("Game ready!").trace
  val chainOfEffectsTwo =
    temporalIO.pure("Loading...").trace *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!").trace

  // Exercise: generalize code
  def timeoutIO[A](ioa: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(ioa, IO.sleep(duration)).flatMap {
      case Left(value) => IO(value)
      case Right(_)    => IO.raiseError(new TimeoutException("computation failed by timeout"))
    }

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using F: Temporal[F]): F[A] =
    F.race(fa, F.sleep(duration)).flatMap {
      case Left(value) => value.pure[F]
      case Right(_)    => F.raiseError(new TimeoutException("computation failed by timeout"))
    }

  override def run: IO[Unit] = chainOfEffectsTwo.void
}
