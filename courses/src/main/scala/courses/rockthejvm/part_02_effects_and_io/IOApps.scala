package courses.rockthejvm.part_02_effects_and_io

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program: IO[Unit] =
    for {
      _    <- IO(println("Hello, please input something:"))
      line <- IO(StdIn.readLine)
      _    <- IO(println(s"Your input is: $line"))
    } yield ()
}

object SimpleApp extends App {
  import IOApps._
  import cats.effect.unsafe.implicits.global

  program.unsafeRunSync()
}

object CatsEffectIOApp extends IOApp {
  import IOApps._

  def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}

object CatsEffectSimpleIOApp extends IOApp.Simple {
  import IOApps._

  def run: IO[Unit] = program
}
