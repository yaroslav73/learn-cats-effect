package books.essential.effects.chapter08

import cats.effect._

object TestingEffects extends IOApp {
  // Comment unsuccessful cases
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(assertGreaterThanOne(IO(10)))
      _ <- IO(assertGreaterThanOne(IO(-10)))
      _ <- IO(assertUnsuccessful(IO(10)))
      _ <- IO(assertUnsuccessful(IO(throw new RuntimeException)))
    } yield ExitCode.Success

  def assertGreaterThanOne(i: IO[Int]): Unit =
    i.map(n => assert(n > 0)).unsafeRunSync()

  def assertUnsuccessful[A](ia: IO[A]): Unit =
    assert(ia.attempt.unsafeRunSync().isRight)
}
