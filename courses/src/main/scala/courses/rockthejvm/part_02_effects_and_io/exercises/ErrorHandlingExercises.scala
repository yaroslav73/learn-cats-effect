package courses.rockthejvm.part_02_effects_and_io.exercises

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import cats.effect.IO

object ErrorHandlingExercises {
  // Construct potentially failed IOs from standard data types (Option, Try, Either)

  // IO.fromOption(option)(ifEmpty)
  def optionToIO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match
      case Some(value) => IO(value)
      case None        => IO.raiseError(ifEmpty)

  // IO.fromTry(t)
  def tryToIO[A](t: Try[A]): IO[A] =
    t match
      case Success(value) => IO(value)
      case Failure(e)     => IO.raiseError(e)

  // IO.fromEither(either)
  def eitherToIO[A](either: Either[Throwable, A]): IO[A] =
    either match
      case Right(value) => IO(value)
      case Left(e)      => IO.raiseError(e)

  // handleError and handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.handleError(handler)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.handleErrorWith(handler)

  def handleIOErrorRedeem[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity) // identity == value => value

  def handleIOErrorRedeemWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)
}
