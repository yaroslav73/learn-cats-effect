package courses.rockthejvm.utils.general

import cats.Functor
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.MonadCancel

extension [F[_], A](fa: F[A])
  def debug(using functor: Functor[F]): F[A] =
    for {
      a <- fa
      t = Thread.currentThread.getName
      _ = println(s"[$t]: $a")
    } yield a

def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
  mc.pure(Thread.sleep(duration.toMillis))
