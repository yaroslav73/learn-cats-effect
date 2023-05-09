package courses.rockthejvm.part_02_effects_and_io

import cats.data.EitherT
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

  def run: IO[Unit] = { // program
    import cats.implicits._

    def print(s: String): IO[Unit] = IO(println(s"${Thread.currentThread().getName}: $s")) // .start.void

    def someMethod(s1: String, s2: Option[String]): EitherT[IO, Int, String] =
      EitherT(IO(s2.toRight(1).map(_ + s1)))

    val result = for {
      s1 <- EitherT.fromOptionF(Option("1").pure[IO], 1)
      s2 = Option("2")
      c <- someMethod(s1, s2)
    } yield print(c)

    result.value.flatMap(_.foldA.start.void)
  }
}
