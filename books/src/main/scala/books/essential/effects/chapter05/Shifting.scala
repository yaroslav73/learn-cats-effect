package books.essential.effects.chapter05

import cats.effect.{ExitCode, IO, IOApp}
import books.essential.effects.debug._

object Shifting extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO("one").debug
      _ <- IO.shift
      _ <- IO("two").debug
      _ <- IO.shift
      _ <- IO("three").debug
    } yield ExitCode.Success
}
