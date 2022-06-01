package courses.rockthejvm.part_02_effects_and_io

import cats.effect.IO

import scala.io.StdIn

object Introduction extends App {

  // IO
  // pure method evaluate eagerly, so should not have side effects
  val simpleIO: IO[Int] = IO.pure(13)
  val shouldNotDoThis: IO[Int] = IO.pure {
    println("DON'T DO THIS!")
    13
  }

  val delayedIO: IO[Int] = IO.delay {
    println("Produce integer...")
    13
  }
  val sameAsDelayedIO: IO[Int] = IO {
    println("Produce integer...")
    13
  }

  // unsafeRunSync need to implicit runtime: unsafe.IORuntime
  // "end of the world", place where you run described computation
  import cats.effect.unsafe.implicits.global
  println(s"Delayed io: ${delayedIO.unsafeRunSync()}")

  import cats.syntax.apply._
  println(s"Result of mapN of two IO[Int]: ${(delayedIO, sameAsDelayedIO).mapN(_ + _).unsafeRunSync()}")

  val program: IO[Unit] =
    for {
      _ <- IO(println("Enter your name:"))
      name <- IO(StdIn.readLine())
      _ <- IO(println("Enter your password:"))
      password <- IO(StdIn.readLine())
      _ <- IO(println(s"$name, your account is crated. Do not show your password ($password) third persons."))
    } yield ()

  println(program.unsafeRunSync())
}
