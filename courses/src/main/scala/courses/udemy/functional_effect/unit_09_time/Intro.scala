package courses.udemy.functional_effect.unit_09_time

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import cats.syntax.traverse.toTraverseOps
import cats.syntax.foldable.toFoldableOps

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object Intro extends IOApp:
  final case class Token(value: String, expirationTimeInMillis: Long):
    def isExpired: IO[Boolean] = IO.realTime.map(_.toMillis > expirationTimeInMillis)

  def measure[A](ioa: IO[A]): IO[FiniteDuration] =
    for {
      start <- IO.monotonic
      _     <- ioa
      end   <- IO.monotonic
    } yield end - start

  def run(args: List[String]): IO[ExitCode] =
    val dummyPrintProgram = (1 to 10000).toList.traverse_ { n =>
      IO.println(n)
    }

    for {
      currentTime <- IO.realTime
      token = Token("token", (currentTime - 1.seconds).toMillis)
      isExpired <- token.isExpired
      _         <- IO.println(s"Is expired: $isExpired")
      _ <- measure(dummyPrintProgram)
        .map(_.toMillis)
        .flatMap(time => IO.println(s"Time execution: $time millis"))
    } yield ExitCode.Success
