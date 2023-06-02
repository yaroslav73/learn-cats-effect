package courses.udemy.functional_effect.unit_09_time

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import java.time.LocalDateTime

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

import java.time.Instant
import java.time.ZoneId

object TimeExercise extends IOApp:
  def tomorrow: IO[FiniteDuration] =
    IO.realTime.map(_.plus(24.hours))

  def tomorrowDateTime: IO[LocalDateTime] =
    tomorrow.map { tomorrow =>
      LocalDateTime.ofInstant(Instant.ofEpochMilli(tomorrow.toMillis), ZoneId.systemDefault())
    }

  def run(args: List[String]): IO[ExitCode] =
    tomorrowDateTime
      .flatTap(t => IO.println(s"Tomorrow is: $t"))
      .as(ExitCode.Success)
