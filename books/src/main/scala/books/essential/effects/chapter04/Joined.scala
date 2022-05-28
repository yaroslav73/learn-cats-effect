package books.essential.effects.chapter04

import cats.effect.{ExitCode, IO, IOApp}
import books.essential.effects.debug._

import scala.concurrent.duration.DurationInt

object Joined extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      fiber <- task.start
      _ <- IO("pre-join").debug
      _ <- fiber.join.debug
      _ <- IO("post-join").debug
    } yield ExitCode.Success

  val task: IO[String] =
    IO.sleep(3.seconds) *> IO("task").debug
  // the *> extension method is equivalent to using mapN with two effects
  // but only the second effect’s value is produced;
  // for example, first *> second is equivalent to (first, second).mapN((_, b) ⇒ b).

  val joined: IO[String] = for {
    fiber <- IO("task").start
    res <- fiber.join
  } yield res
}
