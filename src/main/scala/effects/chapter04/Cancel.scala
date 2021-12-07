package effects.chapter04

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.implicits._
import effects.debug.DebugHelper

// The second thing we can do with a Fiber is to cancel it.
// def cancel: cats.effect.CancelToken[IO]
// type CancelToken[F[_]] = F[Unit]
// Canceling a Fiber is itself an effect.
// It produces a Unit value once the effect is canceled.
object Cancel extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO("pre-cancel").debug
      fiber <- task.onCancel(IO("task was cancelled").debug.void).start
      _ <- fiber.cancel
      _ <- IO("cancelled").debug
    } yield ExitCode.Success

  // IO.never is a built-in non-terminating effect.
  // It has type IO[Nothing], so since type Nothing is a type with no values,
  // this effect can never complete. But it can be cancelled.
  val task: IO[String] = IO("task").debug *> IO.never
}
