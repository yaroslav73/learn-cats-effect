package effects.experiments.io

import cats.effect.IO

object SyncEffects extends App {
  def putStrLn(value: String): IO[Unit] = IO(println(value))

  val readLn: IO[String] = IO(io.StdIn.readLine())

  // interactions with the console in a purely functional way
  val p = for {
    _ <- putStrLn("What's your name?")
    n <- readLn
    _ <- putStrLn(s"Hello, $n! Nice to meet you!")
  } yield ()

  p.unsafeRunSync()
}
