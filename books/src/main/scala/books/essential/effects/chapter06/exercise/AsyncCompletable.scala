package books.essential.effects.chapter06.exercise

import cats.effect.{ExitCode, IO, IOApp}
import books.essential.effects.debug._

import java.util.concurrent.CompletableFuture

import scala.jdk.FunctionConverters._

object AsyncCompletable extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    effect.debug.as(ExitCode.Success)

  val effect: IO[String] =
    fromCF(IO(cf()))

  def fromCF[A](cfa: IO[CompletableFuture[A]]): IO[A] =
    cfa.flatMap { fa =>
      IO.async { cb =>
        val handler: (A, Throwable) => Unit = {
          case (a, null) => cb(Right(a))
          case (null, t) => cb(Left(t))
          case (a, t) =>
            sys.error(
              s"CompletableFuture handler should always have one null, got: $a, $t"
            )
        }

        // handle executes the handler in the current thread,
        // which will be the one executing the function argument to IO.async.
        fa.handle(handler.asJavaBiFunction)

        ()
      }
    }

  // This executes the given function in the ForkJoinPool.commonPool thread pool.
  def cf(): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => "woohoo!!!")
}
