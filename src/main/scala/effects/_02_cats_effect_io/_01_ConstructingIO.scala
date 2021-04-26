package effects._02_cats_effect_io

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object _01_ConstructingIO extends App {
  val hello: IO[Unit] = IO.delay(println("Hello, Cats Effect!"))
  val sameAsHello: IO[Unit] = IO(println("Hello, Cats Effect!"))

  val ohNo: IO[Int] = IO.delay(throw new RuntimeException("NOOOOOOOOOO!!!"))
  val ohNoo: IO[Int] = IO.raiseError(new RuntimeException("NOOOOOOOOOO!!!"))

  val twelve: IO[Int] = IO.pure(12)

  implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  val ioFuture: IO[String] = IO.fromFuture(IO(future))
  val future: Future[String] = Future("This is string")

  ioFuture.map(println)

  ioFuture.map(println).unsafeRunSync()
}
