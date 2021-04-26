package effects._02_cats_effect_io.exercise

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import java.time.{LocalDateTime, LocalTime}
import scala.concurrent.duration.{FiniteDuration, SECONDS}


object TickingClock extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  val tickingClock: IO[Unit] =
    IO.sleep(FiniteDuration(1, SECONDS)).map { _ =>
      val currentTime = LocalTime.now()
      println(s"Current time: $currentTime")
    }.foreverM

  /*

    IO[Unit] {
      while (true) {
        val currentTime = LocalTime.now()
        println(s"Current time: $currentTime")
        IO.sleep(FiniteDuration(1, SECONDS))
      }
    }
  }
   */
}
