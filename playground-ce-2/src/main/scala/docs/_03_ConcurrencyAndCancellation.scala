package docs

import cats.effect.{ContextShift, IO, Timer}

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object _03_ConcurrencyAndCancellation:
  // 1. Not all IO tasks are cancelable. Cancellation status is only checked after asynchronous boundaries.
  // What is asynchronous boundaries?

  private val ec: ExecutionContextExecutor = ExecutionContext.global
  given cs: ContextShift[IO]               = IO.contextShift(ec)

  import DebugHelper.debug
  def retryUntilRight[A, B](io: IO[Either[A, B]], mark: String): IO[B] =
    io.flatMap {
      case Right(b) => IO.pure(b)
      case Left(_) =>
        IO(println(s"[${Thread.currentThread.getName}#$mark]: call retryUntilRight")) *>
          IO.cancelBoundary *>
          retryUntilRight(io, mark)
    }

  // non-terminating IO that is NOT cancelable
  val nonCancelable: IO[Int] = retryUntilRight(IO(Left(0)), "nonCancelable")

  // non-terminating IO that is cancelable because there is an
  // async boundary created by IO.shift before `flatMap` chain
  val cancelable: IO[Int] = IO.shift *> retryUntilRight(IO(Left(0)), "cancelable")

  @main def runExample07(): Unit =
    val p = for {
      fiber1 <- nonCancelable.start
      fiber2 <- cancelable.start
      _      <- IO(println("Start IO..."))
      _      <- IO.timer(ec).sleep(10.millis)
      _      <- IO(println("Attempt to cancel cancelable..."))
      _      <- fiber2.cancel
      _      <- IO(println("Attempt to cancel nonCancelable..."))
      _      <- fiber1.cancel
    } yield ()
    p.unsafeRunSync()

  given timer: Timer[IO]    = IO.timer(ec)
  val runningTask: IO[Unit] = IO(println("Running task...")).flatMap(_ => IO.sleep(1.second) *> runningTask)

  val runningUncancelableTask: IO[Unit] = IO.never

  @main def runExample07_1(): Unit =
    val p = for {
      fiber <- runningTask.start
      _     <- IO.sleep(3.seconds)
      _     <- fiber.cancel
    } yield ()
    runningUncancelableTask.unsafeRunSync()

  // 2. IO tasks that are cancelable, usually become non-terminating on cancel

  // Building cancelable IO tasks

  def sleep(duration: FiniteDuration)(using sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run(): Unit = cb(Right(())) }
      val f = sc.schedule(r, duration.length, duration.unit)

      // Returning a function that can cancel our scheduling
      IO(f.cancel(false)).void
    }

  @main def runExample08(): Unit =
    given ses: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
    val p = for {
      fiber <- sleep(5.seconds).start
      _     <- IO.sleep(3.seconds)
      _     <- fiber.cancel
    } yield ()
    p.unsafeRunSync()
