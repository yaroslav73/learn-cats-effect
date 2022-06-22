package courses.rockthejvm.part_03_concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Try

object AsyncIO extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // IOs can run asynchronously on fibers,
  // without having to manually manage the fiber lifecycle.
  val threadPool: ExecutorService = Executors.newFixedThreadPool(10)
  given ec: ExecutionContext      = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeNumber: Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread.getName}]: Compute number on some other thread...")
    13
  }

  def computeNumberEither: Either[Throwable, Int] = Try { computeNumber }.toEither

  def computeNumberOnThreadPool: Unit = threadPool.execute(() => computeNumberEither)

  // Lift computation to an IO
  // Async is FFI (Foreign Function Interface)
  // 1. CE thread blocks (semantically) until this callback is invoked (by some other thread)
  // 2. Computation not managed by CE
  // 3. CE thread is notified with the result
  val asyncComputationNumberIO: IO[Int] = IO.async_ { callback => // 1
    threadPool.execute { () => // 2
      val result = computeNumberEither
      callback(result) // 3
    }
  }

  // Exercise 1:
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_ { callback =>
    ec.execute { () =>
      val result = Try {
        println(s"[${Thread.currentThread.getName}]: Compute on some other thread...")
        computation()
      }.toEither
      callback(result)
    }
  }

  // Exercise 2: Lift async computation as a Future to an IO.
  def numberFuture: Future[Int] = Future { computeNumber }(ec)

  def futureToIOInt: IO[Int] = IO.async_ { callback =>
    numberFuture.onComplete { result =>
      callback(result.toEither)
    }(ec)
  }

  def futureToIO[A](future: => Future[A]): IO[A] = IO.async_ { (callback: Callback[A]) =>
    future.onComplete { result =>
      callback(result.toEither)
    }
  }

  // Exercise 3: a never-ending IO.
  // No callback - no finish
  // IO.never in CE
  def neverEndingIO[A](computation: () => A): IO[A] = IO.async_ { _ =>
    ec.execute { () =>
      val result = Try(computation()).toEither
      println(s"[${Thread.currentThread.getName}]: compute... $result")
    }
  }

  // Full async call
  def demoAsyncCancellation: IO[Unit] = {
    val asyncNumberIO: IO[Int] = IO.async { (callback: Callback[Int]) =>
      // Why return IO[Option[IO[Unit]]]?
      // Finalizer in case computation gets cancelled.
      // Finalizer are of type IO[Unit]
      // Not specifying finalizer => Option[IO[Unit]]
      // Creating option is an effect => IO[Option[IO[Unit]]
      // as result we got return type IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeNumberEither
          callback(result)
        }
      }.as(Some(IO("Cancelled!").debug.void)) // IO[Unit] => IO[Option[IO[Unit]]
    }

    for {
      fiber <- asyncNumberIO.start
      _     <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fiber.cancel
      _     <- fiber.join
    } yield ()
  }

  override def run: IO[Unit] =
//    asyncComputationNumberIO.debug >>
//      asyncToIO(() => 73)(ec).debug >>
//      futureToIOInt.debug >>
//      futureToIO(numberFuture).debug >>
//      IO.fromFuture(IO(numberFuture)).debug >>
//      neverEndingIO(() => 7).debug >>
    demoAsyncCancellation.debug >>
      IO(threadPool.shutdown())
}
