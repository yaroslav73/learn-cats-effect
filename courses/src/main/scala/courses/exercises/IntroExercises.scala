package courses.exercises

import cats.effect.IO
import cats.Eval

import scala.annotation.tailrec

object IntroExercises {
  // 1. Sequence two IOs and take the result of the LAST one
  // ioa *> iob or ioa >> iob with by-name-call
  def takeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    for {
      _ <- ioa
      b <- iob
    } yield b

  // 2. Sequence two IOs and take the result of the FIRST one
  // ioa <* iob
  def takeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for {
      a <- ioa
      _ <- iob
    } yield a

  // 3. Repeat an IO effect forever
  def forever[A](ioa: IO[A]): IO[A] =
    ioa.flatMap(_ => forever(ioa))

  def foreverLazy[A](ioa: IO[A]): IO[A] =
    ioa >> foreverLazy(ioa)

  // Exception in thread "main" java.lang.StackOverflowError when run it
  def foreverEager[A](ioa: IO[A]): IO[A] =
    ioa *> foreverEager(ioa)

  def foreverM[A](ioa: IO[A]): IO[A] =
    ioa.foreverM

  // 4. Convert and IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    for {
      _ <- ioa
    } yield value

  def convertMap[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convertAs[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  // 5. Discard value inside an IO, just return Unit
  def unit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  // little discourage - don't use this
  def unitAs[A](ioa: IO[A]): IO[Unit] =
    ioa.as(())

  def unitVoid[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  // 6. Fix stack recursion
  def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = IO.defer {
    if (n <= 0) IO(0)
    else sumIO(n - 1).map(n + _)
  }

  def sumIOSolution(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else {
      for {
        lastNumber <- IO(n)
        previousSum <- sumIOSolution(n - 1)
      } yield lastNumber + previousSum
    }

  // 7. Write a fibonacci IO that does not crash on recursion
  def fibonacci(n: Int): IO[BigInt] =
    if (n == 0) IO(0)
    else if (n == 1) IO(1)
    else {
      for {
        a <- IO.defer(fibonacci(n - 1))
        b <- IO.defer(fibonacci(n - 2))
      } yield a + b
    }
}
