package courses.rockthejvm.part_03_concurrency.exercises

import cats.effect.IO
import cats.effect.kernel.Outcome

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

object RaceExercises {
  // Exercises:
  // 1. Implement a timeout pattern with race
  // 2. A method to return a LOSING effect from a race (use racePair)
  // 3. Implement a race in terms of racePair
  def timeout[A](ioa: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(ioa, IO.sleep(duration)).flatMap {
      case Left(value) => IO(value)
      case Right(_)    => IO.raiseError(new TimeoutException("computation failed by timeout"))
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fiber)) =>
        fiber.join.flatMap {
          case Outcome.Succeeded(fb) => fb.map(Right(_))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => IO.raiseError(new RuntimeException("computation canceled"))
        }
      case Right((fiber, _)) =>
        fiber.join.flatMap {
          case Outcome.Succeeded(fa) => fa.map(Left(_))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => IO.raiseError(new RuntimeException("computation canceled"))
        }
    }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB)) =>
        fiberB.cancel >> (outcomeA match {
          case Outcome.Succeeded(fa) => fa.map(Left(_))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => IO.raiseError(new RuntimeException("computation canceled"))
        })
      case Right((fiberA, outcomeB)) =>
        fiberA.cancel >> (outcomeB match {
          case Outcome.Succeeded(fb) => fb.map(Right(_))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => IO.raiseError(new RuntimeException("computation canceled"))
        })
    }

  def simpleRaceAnswer[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB)) =>
        outcomeA match {
          case Outcome.Succeeded(effectA) => fiberB.cancel >> effectA.map(a => Left(a))
          case Outcome.Errored(e)         => fiberB.cancel >> IO.raiseError(e)
          case Outcome.Canceled() =>
            fiberB.join.flatMap {
              case Outcome.Succeeded(effectB) => effectB.map(b => Right(b))
              case Outcome.Errored(e)         => IO.raiseError(e)
              case Outcome.Canceled()         => IO.raiseError(new RuntimeException("Both computation canceled"))
            }
        }
      case Right((fiberA, outcomeB)) =>
        outcomeB match {
          case Outcome.Succeeded(effectB) => fiberA.cancel >> effectB.map(b => Right(b))
          case Outcome.Errored(e)         => fiberA.cancel >> IO.raiseError(e)
          case Outcome.Canceled() =>
            fiberA.join.flatMap {
              case Outcome.Succeeded(effectA) => effectA.map(a => Left(a))
              case Outcome.Errored(e)         => IO.raiseError(e)
              case Outcome.Canceled()         => IO.raiseError(new RuntimeException("Both computation canceled"))
            }
        }
    }
}
