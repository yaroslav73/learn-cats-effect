package effects

import cats.effect.IO

object debug {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def debug: IO[A] =
      for {
        a <- ioa
        thread = Thread.currentThread.getName
        _ = println(s"[${Console.GREEN + thread + Console.RESET}]: $a")
      } yield a
  }
}
