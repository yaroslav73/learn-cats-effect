package effects.chapter07

import cats.effect.{ExitCode, IO, IOApp, Resource}
import effects.debug._

object BasicResourceFailure extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    BasicResource.stringResource
      .use(_ => IO.raiseError(new RuntimeException("oh noes!")))
      .attempt
      .debug
      .as(ExitCode.Success)
}
