package books.essential.effects.chapter03

import cats.{Parallel, effect}
import cats.effect._
import cats.implicits._

object Parallel02 extends IOApp {
  val hello: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: Hello")
  )
  val world: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: World")
  )

  // Using Parallel to translate between IO and IO.Par
  val helloPar: IO.Par[Unit] = Parallel[IO].parallel(hello)
  val worldPar: IO.Par[Unit] = Parallel[IO].parallel(world)

  val hw01: effect.IO.Par[Unit] = (helloPar, worldPar).mapN((_, _) => ())

  // Replacing the explicit sequential -> parallel ->
  // sequential transformation with the parMapN method.
  val hw02: IO[Unit] = (hello, world).parMapN((_, _) => ())

  override def run(args: List[String]): IO[ExitCode] =
    Parallel[IO].sequential(hw01) *> hw02.as(ExitCode.Success)
}
