package books.essential.effects.experiments.io

import cats.effect.{ContextShift, IO}

import scala.concurrent.{ExecutionContext, Future}

object DeferredExecutionEffects extends App {
  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO.defer {
      if (n > 0) fib(n - 1, b, a + b)
      else IO.pure(a)
    }

  println(fib(1000).unsafeRunSync())

  def fibs(n: Int, a: Long = 0, b: Long = 1)(implicit
      cs: ContextShift[IO]
  ): IO[Long] =
    IO.defer {
      if (n == 0) IO.pure(a)
      else {
        val next = fibs(n - 1, b, a + b)
        // Every 100 cycles, introduce a logical thread fork
        if (n % 100 == 0) {
          println("Before shift: " + Thread.currentThread())
          cs.shift *> next
        } else next
      }
    }

  implicit val ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  println(fibs(1000).unsafeRunSync())
}
