package effects.chapter02

import cats.effect.{ExitCode, IO, IOApp}

object IOApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    helloEffects.as(ExitCode.Success)

  val helloEffects: IO[Unit] =
    IO(println("Hello, effects! From IOApp!"))
}
