package effects.chapter02.exercise

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import java.time.LocalTime
import scala.concurrent.duration.DurationInt
import effects.debug._

object Exercise extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  // tickingClock should print the current time every second.
  // Use System.currentTimeMillis to be simple.
  // How do you wake up every second? (Hint: You can use IO.sleep.)
  // And how do you do this repeatedly?
  val tickingClock: IO[Unit] = for {
    _ <- IO.sleep(1.seconds)
    _ <- IO(s"Now is: ${LocalTime.now()}").debug
    _ <- tickingClock
  } yield ()

  val anotherTickingClock: IO[Unit] = IO
    .sleep(1.seconds)
    .flatMap { _ =>
      IO(println(s"Now is: ${LocalTime.now()}"))
    }
    .foreverM
}
