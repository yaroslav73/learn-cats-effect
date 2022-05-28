package books.essential.effects.experiments.io

import cats.effect.IO

object HelloExample extends App {
  val ioa = IO { println("Hello, effects!") }

  val program: IO[Unit] = for {
    _ <- ioa
    _ <- ioa
  } yield ()

  program.unsafeRunSync()
}
