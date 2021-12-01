package effects.chapter02

import cats.effect.IO
import cats.implicits._

object ErrorHandling extends App {
  val defaultIntValue = 13

  // IO computation can fail, either by throwing an exception during execution
  // Capturing an existing exception via IO.raiseError
  val ioWithError = IO.raiseError[Int](new RuntimeException("Oh, no..."))

  // Handle the error by producing a new effect:
  val handleIOWithError =
    ioWithError.handleErrorWith(e =>
      IO {
        print(s"Error was happened: ${e.getMessage}, use default value: ")
        defaultIntValue
      }
    )

  // Or simply provide a successful value, use handleError:
  val sameAsAbove01 = ioWithError.handleError(_ => defaultIntValue)

  println(handleIOWithError.unsafeRunSync())
  println(sameAsAbove01.unsafeRunSync())

  //
  val handleWithError: IO[Int] =
    ioWithError.handleErrorWith(t => IO.raiseError(new OtherException(t)))

  // To explicitly transform the error into another error, use adaptError instead:
  val sameAsAbove02 = ioWithError.adaptError(t => new OtherException(t))

  // Instead of hiding the error-handling - now exposing the error,
  // but also delaying the error-handling by “lifting” the error
  // into a (successful) IO value:
  val result: IO[Either[Throwable, Int]] =
    handleWithError
      .map(n => Right(n))
      .handleErrorWith(t => IO(Left(t)))

  val sameAsAbove03 = handleWithError.attempt

  println(sameAsAbove03.unsafeRunSync())

  class OtherException(throwable: Throwable) extends Exception(throwable)
}
