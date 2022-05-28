package books.essential.effects.experiments.io

import cats.effect.IO

object StackSafetyExample extends App {
  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if (n > 0) fib(n - 1, b, b2)
      else IO.pure(a)
    }

  println(fib(100).unsafeRunSync())
}
