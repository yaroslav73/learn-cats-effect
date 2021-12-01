package effects.chapter03

import cats.effect._
import cats.implicits._
import scala.concurrent.ExecutionContext

object _02_Parallel01 extends App {
  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val hello: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: Hello")
  )
  val world: IO[Unit] = IO(
    println(s"[${Thread.currentThread().getName}]: World")
  )

  val helloPar = IO.Par(hello)
  val worldPar = IO.Par(world)

  val hw01 = (helloPar, worldPar).mapN((_, _) => ())

  // We can use
  // import scala.concurrent.ExecutionContext
  // and
  // implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  // or just extends IOApp
  // and override run method:
  // override def run(args: List[String]): IO[ExitCode] =
  //   IO.Par.unwrap(hw01).as(ExitCode.Success)

  IO.Par.unwrap(hw01).unsafeRunSync()
}
