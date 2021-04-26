package effects._02_cats_effect_io

import cats.effect.IO
import cats.implicits._

object _03_ErrorHandling extends App {
  val defaultIntValue = 13

  val ioWithError = IO.raiseError[Int](new RuntimeException("Oh, no..."))

  val handleIOWithError = ioWithError.handleErrorWith(_ => IO(defaultIntValue))
  val sameAsAbove01 = ioWithError.handleError(_ => defaultIntValue)

  println(handleIOWithError.unsafeRunSync())

  val handleWithError: IO[Int] = ioWithError.handleErrorWith(t => IO.raiseError(new OtherException(t)))
  val sameAsAbove02 = ioWithError.adaptError(t => new OtherException(t))

  val result: IO[Either[Throwable, Int]] =
    handleWithError
      .map(n => Right(n))
      .handleErrorWith(t => IO(Left(t)))

  val sameAsAbove03 = handleWithError.attempt

  println(sameAsAbove03.unsafeRunSync())

  class OtherException(throwable: Throwable) extends Exception(throwable)
}
