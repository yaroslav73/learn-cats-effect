package books.essential.effects.chapter02

import cats.effect.{ExitCode, IO, IOApp}

object IOAppExample extends IOApp {
  // The application executes them when we run it.
  override def run(args: List[String]): IO[ExitCode] = {
    // Declare the computations that will be run.
    helloEffects.as(ExitCode.Success)
  }

  // Describe the effects we want to happen
  val helloEffects: IO[Unit] =
    IO(println("Hello, effects! From IOApp!"))
}
