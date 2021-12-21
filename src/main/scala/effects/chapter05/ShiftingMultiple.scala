package effects.chapter05

import cats.effect.{ExitCode, IO, IOApp}
import effects.debug._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ShiftingMultiple extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    // 1. Construct two new ExecutionContext values to use.
    (ec("1"), ec("2")) match {
      case (ec1, ec2) =>
        for {
          // 2. By default, our computations will execute on the ExecutionContext of our IOApp.
          _ <- IO("one").debug
          // 3. Shift onto another ExecutionContext for the next effect.
          _ <- IO.shift(ec1)
          _ <- IO("two").debug
          // 4. Next shift onto the second ExecutionContext.
          _ <- IO.shift(ec2)
          _ <- IO("three").debug
        } yield ExitCode.Success
    }
  }

  // 5. Create a new single-threaded ExecutionContext.
  def ec(name: String): ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r =>
      val t = new Thread(r, s"pool-$name-thread-1")
      // 6. We need daemon threads so the JVM shuts down correctly.
      t.setDaemon(true)
      t
    })
}
