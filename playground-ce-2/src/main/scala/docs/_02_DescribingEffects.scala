package docs

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext
//import cats.effect.unsafe.implicits.global

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Random, Success}
//import cats.effect.kernel.{Async, GenSpawn}

object _02_DescribingEffects:
  // IO is a potent abstraction that can efficiently describe multiple kinds of effects:
  // 1. Pure Values — IO.pure & IO.unit
  // Lift pure values into IO, yielding IO values that are "already evaluated"
  val pureAndApply = IO.pure(13).flatMap(n => IO(println(s"The number is: $n")))

  val unit = IO.unit

  // 2. Synchronous Effects — IO.apply
  // Note the given parameter is passed ''by name'', its execution being "suspended" in the IO context.
  def putStrLn(s: String) = IO(println(s))
  val readLn              = IO(scala.io.StdIn.readLine())

  val simpleConsoleApp =
    for {
      _ <- putStrLn("What is your name?")
      n <- readLn
      _ <- putStrLn(s"Hello $n!")
    } yield ()

  // 3. Asynchronous Effects — IO.async & IO.cancelable
  // IO can describe asynchronous processes via the IO.async and IO.cancelable builders.

  // An asynchronous task represents logic that executes independent of the main program flow, or current callstack.
  // It can be a task whose result gets computed on another thread, or on some other machine on the network.
  val futureSuccess = Future.successful("Success return!")
  val futureFailure = Future.failed[String](new java.lang.IllegalArgumentException("Wrong parameter!"))

  import concurrent.ExecutionContext.Implicits.global

  def asyncCall[A](body: => Future[A]): IO[A] =
    IO.async { cb =>
      println(s"Compute async_ on ${Thread.currentThread.getName}")
      body.onComplete {
        case Success(value) =>
          println(s"Compute cb on ${Thread.currentThread.getName}")
          cb(Right(value))
        case Failure(error) =>
          println(s"Compute cb on ${Thread.currentThread.getName}")
          cb(Left(error))
      }
    }

  // Cancelable Processes
  // Important: cancellation is the ability to interrupt an IO task before completion,
  // possibly releasing any acquired resources, useful in race conditions to prevent leaks.
  def delayTick(d: FiniteDuration)(using sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = cb(Right(())) }
      val f = sc.schedule(r, d.length, d.unit)

      // Returning the cancellation token needed to cancel
      // the scheduling and release resources early
      IO(f.cancel(false)).void
    }

  // IO.never
  // Represents a non-terminating IO defined in terms of async
  val never: IO[Nothing] = IO.async(_ => IO(Option.empty[IO[Unit]])) // Is the same as IO.never
  val n                  = IO.never

  // 4. Deferred Execution — IO.defer (previously suspend)
  // It's also useful for modeling stack safe, tail recursive loops:
  val defereTen = IO.defer(IO(10)) // The same as IO(???).flatten

  def fib(n: Int, a: Long, b: Long): IO[Long] =
    IO.defer {
      if n > 0 then fib(n - 1, b, a + b)
      else IO.pure(a)
    }

  import DebugHelper.debug
  def fibConcurrent(n: Int, a: Long, b: Long)(using cs: ContextShift[IO]): IO[Long] =
    IO.defer {
      if n == 0 then IO.pure(a)
      else
        val next = fib(n - 1, b, a + b)
        // Every 100 cycles, introduce a logical thread fork
        // TODO: For now I don't understand how IO.cede works :/ Need to learn more.
        if n % 100 == 0 then cs.shift.debug *> next
        else next.debug // .debug(s"${Thread.currentThread.getName}")
    }

  def fibNotSafe(n: Int, a: Long, b: Long): IO[Long] =
    if n > 0 then fib(n - 1, b, a + b)
    else IO.pure(a)

  @main def runExample02(): Unit =
    pureAndApply.unsafeRunSync()

    unit.flatMap(v => IO(println(v))).unsafeRunSync()

  @main def runExample03(): Unit =
    simpleConsoleApp.unsafeRunSync()

  @main def runExample04(): Unit =
    asyncCall(futureSuccess).map(println).unsafeRunSync()
    asyncCall(futureFailure).map(println).unsafeRunSync()

  @main def runExample05(): Unit =
    given sc: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
    delayTick(5.seconds).unsafeRunSync()

  @main def runExample06(): Unit =
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    fibConcurrent(1000, 0, 1).flatMap(n => IO(println(n))).unsafeRunSync() // Async(_ => ())

// TODO: iterateWhile does not exit in CE2?
//  @main def checkIterateWhile(): Unit =
//    IO(Random.nextInt(15))
//      .iterateWhile { n =>
//        println(n)
//        n != 10
//      }
//      .map(println)
//      .unsafeRunSync()
