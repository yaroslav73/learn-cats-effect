package docs

import cats.effect.{ContextShift, IO}

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object _03_ConcurrencyAndCancellation:
  // 1. Not all IO tasks are cancelable. Cancellation status is only checked after asynchronous boundaries.
  // What is asynchronous boundaries?

  private val ec: ExecutionContextExecutor = ExecutionContext.global
  implicit val cs: ContextShift[IO]        = IO.contextShift(ec)

  def retryUntilRight[A, B](io: IO[Either[A, B]]): IO[B] =
    io.flatMap {
      case Right(b) => IO.pure(b)
      case Left(_) =>
        IO(println(s"[${Thread.currentThread.getName}]: call retryUntilRight again")) *> retryUntilRight(io)
    }

  // non-terminating IO that is NOT cancelable
  val nonCancelable: IO[Int] = retryUntilRight(IO(Left(0)))

  // non-terminating IO that is cancelable because there is an
  // async boundary created by IO.shift before `flatMap` chain
  val cancelable: IO[Int] = IO.shift *> retryUntilRight(IO(Left(0)))

  @main def runExample07(): Unit =
    val p = for {
      fiber <- cancelable.start
      _     <- IO.timer(ec).sleep(2.seconds)
      _     <- fiber.cancel
      _     <- IO(println("Fiber canceled!"))
    } yield ()
    p.unsafeRunSync()

  // 2. IO tasks that are cancelable, usually become non-terminating on cancel

  // Building cancelable IO tasks

  def sleep(duration: FiniteDuration)(using sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = cb(Right(())) }
      val f = sc.schedule(r, duration.length, duration.unit)

      // Returning a function that can cancel our scheduling
      IO(f.cancel(false)).void
    }

  @main def runExample08(): Unit =
    implicit val ses: ScheduledExecutorService = Executors.newScheduledThreadPool(1) 
    sleep(30.seconds)
