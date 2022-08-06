package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.Sync
import cats.effect.Temporal
import cats.effect.Async
import cats.effect.Concurrent
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import cats.syntax.monoid.*
import cats.syntax.applicative.*

object PolymorphicAsync extends IOApp.Simple {

  // Async - asynchronous computation, "suspended" in F
  trait _Async[F[_]] extends Sync[F] with Temporal[F] {
    // Fundamental description of async computation
    def executionContext: F[ExecutionContext]
    def async[A](callback: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ex: ExecutionContext): F[A]

    // async_ get for free
    def async_[A](callback: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(cb => map(pure(callback(cb)))(_ => None))

    // Never-ending effect
    def never[A]: F[A] = async(_ => pure(Option(pure(())))) // or async_(_ => ())
  }

  val asyncIO = Async[IO] // given Async[IO] in scope

  // Abilities: pure, map/flatMap, raiseError, uncancelable,
  // start, ref/deferred, sleep, delay/defer/blocking
  // +
  val ec = asyncIO.executionContext

  // Power: async_ + async
  val threadPool = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit

  val asyncNumber73: IO[Int] = IO.async_ { (callback: Callback[Int]) =>
    // Start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}]: computing an async number...")
      callback(Right(73))
    }
  }

  // Same as above
  val asyncNumber73Same: IO[Int] = asyncIO.async_ { (callback: Callback[Int]) =>
    // Start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}]: computing an async number...")
      callback(Right(73))
    }
  }

  import courses.rockthejvm.utils.general.*

  val asyncNumber73Complex: IO[Int] = IO.async { (callback: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}]: computing an async number...")
        callback(Right(73))
      }
    }.as(Some(IO("Cancelled!").debug.void)) // Finalizer in case the computation gets cancelled
  }

  // Same as above
  val asyncNumber73ComplexSame: IO[Int] = asyncIO.async { (callback: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}]: computing an async number...")
        callback(Right(73))
      }
    }.as(Some(IO("Cancelled!").debug.void)) // Finalizer in case the computation gets cancelled
  }

  val _excecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncNumber73Again = asyncIO.evalOn(IO(73), _excecutionContext).guarantee(IO(threadPool.shutdown()))

  // Never-ending
  val neverEndingAsyncIO = asyncIO.never

  // Exercises:
  // 1. Implement never and async_ in terms of the big async.
  // 2. Tuple two effects with different requirements.

  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_]: Sync, A](a: A): F[A]      = Sync[F].pure(a)

  import cats.syntax.functor.* // map extension method
  import cats.syntax.flatMap.* // flatMap extension method
  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] =
    for {
      first  <- firstEffect(a)
      second <- secondEffect(a)
    } yield (first, second)

  def run: IO[Unit] = ???
}
