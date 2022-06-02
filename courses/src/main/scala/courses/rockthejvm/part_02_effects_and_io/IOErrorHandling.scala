package courses.rockthejvm.part_02_effects_and_io

import cats.effect.IO

object IOErrorHandling extends App {
  // IO: pure, delay, defer
  // What about failed effects?
  val failedComputation: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE!"))
  val failure: IO[Int] = IO.raiseError(new RuntimeException("A proper fail!"))

  // How to handle failure?
  val dealWithIt = failure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("Failure happens, but I still here!"))
    // You can add more cases to handle
  }

  // Turn into Either
  val effectAsEither: IO[Either[Throwable, Int]] = failedComputation.attempt

  // redeem - transform the failure and the success in one step
  val resultAsString: IO[String] = failure.redeem(e => s"Fail: $e", r => s"Success: $r")

  // redeemWith
  val resultAsEffect: IO[Unit] = failure.redeemWith(e => IO(println(s"Fail: $e")), r => IO(println(s"Success: $r")))


  import cats.effect.unsafe.implicits.global
  resultAsEffect.unsafeRunSync()
}
