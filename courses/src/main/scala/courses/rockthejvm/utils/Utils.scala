package courses.rockthejvm.utils

import cats.effect.IO

extension [A](ioa: IO[A])
  def trace: IO[A] =
    for {
      a <- ioa
      t = Thread.currentThread.getName
      g = Console.GREEN
      r = Console.RESET
      _ = println(s"[$g$t$r]: $a")
    } yield a
