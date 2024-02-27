package blogs

import cats.effect.*
import scala.concurrent.duration.*
import cats.syntax.functor.toFunctorOps
import cats.effect.std.Supervisor
import cats.effect.syntax.supervisor

// https://softwaremill.com/concurrency-with-cats-effect/
object ConcurrencyWithCE extends IOApp.Simple:
  // A basic building block of CE concurrency is fiber.
  // In a nutshell, fiber is a lightweight logical thread that represents a sequence of actions.
  // In the case of CE, it’s a list of operations suspended with IO monad and sequenced with flatMap.

  // Unlike system threads, fibers do not rely on the operating system's scheduler to switch between contexts.

  // 1. Managing fibers
  val task1 =
    for {
      _ <- IO.println(s"[${Thread.currentThread.getName}]: Task started")
      _ <- IO.sleep(1.second)
      _ <- IO.println(s"[${Thread.currentThread.getName}]: Task finished")
    } yield ()

  // .start return
  // IO[FiberIO[A @uncheckedVariance]], where FiberIO is aliase for
  // type FiberIO[A] = Fiber[IO, Throwable, A]

  // Fiber trait has two general method - join and cancel.
  val task2 =
    for {
      fiber <- task1.start
      _     <- IO.println(s"[${Thread.currentThread.getName}]: Hello from main fiber")
      // We wait approximately 1 second till forked fiber finishes
      _ <- fiber.join
    } yield ()

  val task3 =
    for {
      _ <- IO.println(s"[${Thread.currentThread.getName}]: Computations started")
      _ <- IO.sleep(5.seconds)
      _ <- IO.println(s"[${Thread.currentThread.getName}]: Computations completed")
    } yield ()

  val task4 =
    for {
      fiber <- task3.onCancel(IO.println(s"[${Thread.currentThread.getName}]: Task cancelled")).start
      _     <- IO.sleep(1.second)
      // Fiber would complete after 5 seconds, but is cancelled after 1 second
      _ <- fiber.cancel
    } yield ()

  // The .join method returns Outcome, that can be Succeeded, Errored or Canceled
  val task5 =
    IO.sleep(500.millis) *>
      IO.pure("Hello, World!")
    // IO.canceled
    // IO.raiseError(new RuntimeException("Boom!"))

  val task6 =
    for {
      fiber <- task5.start
      _ <- fiber.join.flatMap {
        case Outcome.Succeeded(value) => value.flatMap(v => IO.println(s"Computed value: $v"))
        case Outcome.Errored(e)       => IO.println(s"Error: ${e.getMessage}")
        case Outcome.Canceled()       => IO.println("Task was cancelled")
      }
    } yield ()

  // Method join comes with another variant: joinWith(onCancel: F[A]).
  // It allows specifying a fallback computation in case the fiber is canceled.
  // It has two specialized variations: joinWithNever,
  // which is a shortcut for joinWith(IO.never),
  // and joinWithUnit (a shortcut for joinWith(IO.unit)).

  // 2. Timeouts
  val task7 =
    for {
      _ <- task3
        .onCancel(IO.println(s"[${Thread.currentThread.getName}]: Task has time out!"))
        .timeout(500.millis)
        .start
      _ <- IO.sleep(1.second)
    } yield ()

  val task8 =
    for {
      _ <- task3
        .onCancel(IO.println(s"[${Thread.currentThread.getName}]: Task has time out!"))
        .timeoutTo(500.millis, IO.println(s"[${Thread.currentThread.getName}]: Plan B if main task timed out!"))
        .start
      _ <- IO.sleep(1.second)
    } yield ()

  val task9 =
    for {
      _ <- task3
        .onCancel(IO.println(s"[${Thread.currentThread.getName}]: Task has time out!"))
        .timeoutAndForget(500.millis)
        .start
      _ <- IO.sleep(1.second)
    } yield ()

  // 3. Structured concurrency
  // If fiber is not joined or canceled, it will run parallel to the call site fiber.
  // It will end only if its computations are complete or raise an error.
  // If the computation running on fiber never ends, the fiber won’t ever complete.
  val task10 =
    for {
      time <- IO.realTimeInstant
      _    <- IO.println(s"[${Thread.currentThread.getName}]: Current date and time: $time")
      _    <- IO.sleep(1.second)
    } yield ()

  val task11 =
    for {
      _ <- task10.foreverM.start // task is running as an infinite loop and fiber is never canceled
      _ <- IO.sleep(5.seconds)
      _ <- IO.println("Bye!")
    } yield ()

  val backgroundTask: Resource[IO, Unit] =
    task10.foreverM
      .onCancel(IO.println(s"[${Thread.currentThread.getName}]: Closing the backgound task!"))
      .background
      .void

  val task12 =
    backgroundTask.surround {
      for {
        _ <- IO.sleep(5.seconds)
        _ <- IO.println("Bye!")
        // after foreground task is complete the background fiber will be canceled
      } yield ()
    }

  val task13 = Supervisor[IO](await = false).use { supervisor =>
    for {
      _ <- supervisor.supervise(IO.println("A").andWait(1.second).foreverM)
      _ <- supervisor.supervise(IO.println("B").andWait(2.seconds).foreverM)
      _ <- IO.sleep(5.seconds) // fibers will print A and B for 5 seconds and then terminate
    } yield ()
  }

  import cats.effect.implicits.*
  val task14 =
    Supervisor[IO](await = true).use { supervisor =>
      IO.println("Hello")
        .andWait(1.second)
        .foreverM
        .timeout(5.seconds)
        .supervise(supervisor)
    }

  val task15 =
    for {
      _ <- task14
    } yield ()

  // 4. Concurrency operators
  val raced = IO.race(
    IO.println("Faster").delayBy(3.seconds),
    IO.println(73).delayBy(5.seconds)
  )

  val task16 =
    for {
      _ <- raced
    } yield ()

  val recedPair =
    IO.racePair(
      IO(13).delayBy(3.seconds),
      IO(73).delayBy(5.seconds)
    )
    // .flatMap {
    // case Left(leftResut, rightFiber)   => rightFiber.cancel *> IO.println(s"Left result: $leftResut")
    // case Right(leftFiber, rightResult) => leftFiber.cancel *> IO.println(s"Right result: $rightResult")
    // }

  val task17 =
    for {
      result <- recedPair
      _ <- result match {
        case Left(leftResut, rightFiber)   => rightFiber.cancel *> IO.println(s"Left result: $leftResut")
        case Right(leftFiber, rightResult) => leftFiber.cancel *> IO.println(s"Right result: $rightResult")
      }
    } yield ()

  //  If any of the operations is canceled or fails the second one will also be canceled.
  val task18 = IO.both(IO(13).delayBy(1.second), IO("Hello!"))

  val task19 =
    for {
      result <- task18
      _      <- IO.println(s"Computations result: $result")
    } yield ()

  import cats.syntax.parallel.catsSyntaxParallelSequence1
  val results = List(
    IO(1).delayBy(1.second),
    IO(2).delayBy(2.seconds),
    IO(3).delayBy(3.seconds)
  ).parSequence

  val task20 =
    for {
      result <- results
      _      <- IO.println(s"Computations result: $result")
    } yield ()

  def run: IO[Unit] =
    // task2
    // task4
    // task6
    // task7
    // task8
    // task9
    // task11
    // task12
    // task13
    // task15
    // task16
    // task17
    // task19
    task20
