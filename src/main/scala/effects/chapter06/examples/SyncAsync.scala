package effects.chapter06.examples

import cats.effect.{ExitCode, IO, IOApp}
import effects.debug._

object SyncAsync extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- synchronousSum(7, 5).debug
    } yield ExitCode.Success

  def synchronousSum(a: Int, b: Int): IO[Int] =
    IO.async { cb =>
      cb(Right(a + b))
    }
}
