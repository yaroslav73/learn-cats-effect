package effects.chapter03

import cats.Parallel
import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext

object _02_Parallel02 extends IOApp {
  val hello: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: Hello")
  )
  val world: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: World")
  )

  // val helloPar = Parallel[IO].parallel(hello)
  // val worldPar = Parallel[IO].parallel(world)
  //
  // val hw01 = (helloPar, worldPar).mapN((_, _) => ())

  // override def run(args: List[String]): IO[ExitCode] =
  //   Parallel[IO].sequential(hw01).as(ExitCode.Success)

  // Or we can compress code above to
  val hw01 = (hello, world).parMapN((_, _) => ())

  override def run(args: List[String]): IO[ExitCode] =
    hw01.as(ExitCode.Success)
}
