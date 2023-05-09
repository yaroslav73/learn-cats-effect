package courses.rockthejvm.part_03_concurrency

import cats.effect.IOApp
import cats.effect.IO

import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object BlockingIO extends IOApp.Simple {
  import courses.rockthejvm.utils._

  val someSleeps = for {
    _ <- IO.sleep(1.second).trace
    _ <- IO.sleep(1.second).trace
  } yield ()

  // Really blocking IO
  // Will evalute on a thread from ANOTHER thread pool specific for blocking calls
  val someBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread.getName}]: Computed a blocking code")
    13
  }

  // Yielding
  // IO.ced - a signal to yield control over the thread - equivalent to IO.shift
  // The reast of the effect may run on another thread (not necessarily)
  val iosOnManyThreads = for {
    _ <- IO("First").trace
    _ <- IO.cede
    _ <- IO("Second").trace
    _ <- IO.cede
    _ <- IO("Third").trace
  } yield ()

  def testThousandEffectsSwitch = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.trace >> IO.cede >> _.trace).evalOn(ec)
  }

  // Blocking calls and IO.sleep and yield control over the calling thread automatically.

  override def run: IO[Unit] = testThousandEffectsSwitch.void
}
