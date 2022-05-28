package books.essential.effects.chapter07

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxTuple2Parallel, catsSyntaxTuple2Semigroupal}
import books.essential.effects.debug._

object BasicResourceComposed extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    // Can try to change on tupled and check output
    (stringResource, intResource).parTupled
      .use { case (s, i) =>
        IO(s"$s is so cool!").debug *>
          IO(s"$i is also cool!").debug
      }
      .as(ExitCode.Success)
  }

  val stringResource: Resource[IO, String] =
    Resource.make(IO("> acquiring stringResource").debug *> IO("String"))(_ =>
      IO("< releasing stringResource").debug.void
    )

  val intResource: Resource[IO, Int] =
    Resource.make(IO("> acquiring intResource").debug *> IO(101))(_ =>
      IO("< releasing intResource").debug.void
    )
}
