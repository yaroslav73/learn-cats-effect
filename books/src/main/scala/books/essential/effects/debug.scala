package books.essential.effects

import cats.effect.IO

import java.time.LocalTime

object debug {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def debug: IO[A] =
      for {
        a <- ioa
        t = Thread.currentThread.getName
        g = Console.GREEN
        r = Console.RESET
        _ = println(s"[${LocalTime.now}, $g$t$r]: $a")
      } yield a
  }
}
