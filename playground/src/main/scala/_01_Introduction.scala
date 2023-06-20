import scala.concurrent.Future
import cats.effect.IO

object _01_Introduction {
  // An IO is a data structure that represents just a description
  // of a side effectful computation.

  val ioa = IO { println("Hey!") }

  val firstIOProgram =
    for {
      _ <- ioa
      _ <- ioa
    } yield ()

  /*
   *                      Eager                       Lazy
   * Synchronous            A                        () => A
   *                                                 Eval[A]
   * Asynchronous   (A => Unit) => Unit     () => (A => Unit) => Unit
   *                     Future[A]                    IO[A]
   */

  import scala.concurrent.ExecutionContext.Implicits.global

  def addToFuture(n: Int): Future[Int] = Future {
    println(s"[Future] Add $n to 1")
    1 + n
  }

  // unsafeRunSync() need implicit IORuntime, so we import it
  import cats.effect.unsafe.implicits.global

  def addToIO(n: Int): IO[Int] = IO {
    println(s"[IO] Add $n to 1")
    1 + n
  }

  val futureTask = addToFuture(14)
  val ioTask     = addToIO(14)

  // Stack Safety
  // IO is trampolined in its flatMap evaluation.
  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if n > 0 then fib(n - 1, b, b2)
      else IO.pure(a)
    }

  @main def runExamples01(): Unit =
    // firstIOProgram, doesn't execute until we run it with unsafeRun... methodss
    firstIOProgram.unsafeRunSync()

    // Future
    for {
      _ <- addToFuture(13)
      _ <- addToFuture(13)
    } yield ()

    // IO
    val program =
      for {
        _ <- addToIO(13)
        _ <- addToIO(13)
      } yield ()

    program.unsafeRunSync()

    for {
      _ <- futureTask
      _ <- futureTask
    } yield ()

    val tasks =
      for {
        _ <- ioTask
        _ <- ioTask
      } yield ()

    tasks.unsafeRunSync()

    fib(7).flatMap(IO.println).unsafeRunSync()
}
