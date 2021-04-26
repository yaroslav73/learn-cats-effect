package effects._02_cats_effect_io

import cats.effect.{ExitCode, IO, IOApp}

object _03_IOApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    helloEffects.as(ExitCode.Success)

  val helloEffects: IO[Unit] =
    IO(println("Hello, effects! From IOApp!"))
}
