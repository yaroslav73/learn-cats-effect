package effects.chapter07

import cats.effect.{ExitCode, IO, IOApp, Resource}
import effects.debug._

object BasicResource extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    stringResource
      .use { s =>
        IO(s"$s is so cool!").debug
      }
      .as(ExitCode.Success)

  val stringResource: Resource[IO, String] =
    Resource.make(IO("> acquiring stringResource").debug *> IO("String"))(_ =>
      IO("< releasing stringResource").debug.void
    )
}
