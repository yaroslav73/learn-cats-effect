package courses.rockthejvm.utils.general

import cats.Functor
import cats.syntax.flatMap.*
import cats.syntax.functor.*

extension [F[_], A](fa: F[A])
  def debug(using functor: Functor[F]): F[A] =
    for {
      a <- fa
      t = Thread.currentThread.getName
      _ = println(s"[$t]: $a")
    } yield a
