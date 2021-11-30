package effects.experiments.io

import cats.effect.IO

object DescribingEffects extends App {
  // IO.pure - given parameter is passed by value (eagerly evaluated)
  // IO.apply -given parameter is passed by name (lazily evaluated)
  IO.pure(73).flatMap(n => IO(println(s"Number is: $n"))).unsafeRunSync()

  val pureIO = IO.pure(println("This is WRONG!!!"))
  // How many times was printing?
  val p1 = for {
    _ <- pureIO
    _ <- pureIO
  } yield ()
  p1.unsafeRunSync()

  val io: IO[Unit] = IO(println("Correct?"))
  // How many times was printing?
  val p2 = for {
    _ <- io
    _ <- io
  } yield ()
  p2.unsafeRunSync()
}
