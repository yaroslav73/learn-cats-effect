package courses.rockthejvm.part_04_concurrent_coordination

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.std.Semaphore
import cats.syntax.parallel.*
import scala.concurrent.duration.*
import scala.util.Random

object Semaphores extends IOApp.Simple {
  import courses.rockthejvm.utils.*

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // Example: limiting the number of concurrent sessions on a server
  def doWorkWhileLoggedIn: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, semaphore: Semaphore[IO]): IO[Int] =
    for {
      _ <- IO(s"[Session $id] waiting to log in...").trace
      _ <- semaphore.acquire
      // Critical section
      _   <- IO(s"[Session $id] logged in, working...").trace
      res <- doWorkWhileLoggedIn
      _   <- IO(s"[Session $id] done: $res, logging out...").trace
      // End of critial section
      _ <- semaphore.release
    } yield res

  def demoSemaphore: IO[Unit] =
    for {
      semaphore  <- Semaphore[IO](2)
      user1Fiber <- login(1, semaphore).start
      user2Fiber <- login(2, semaphore).start
      user3Fiber <- login(3, semaphore).start
      _          <- user1Fiber.join
      _          <- user2Fiber.join
      _          <- user3Fiber.join
    } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, semaphore: Semaphore[IO]): IO[Int] =
    for {
      _ <- IO(s"[Session $id] waiting to log in...").trace
      _ <- semaphore.acquireN(requiredPermits)
      // Critical section
      _   <- IO(s"[Session $id] logged in, working...").trace
      res <- doWorkWhileLoggedIn
      _   <- IO(s"[Session $id] done: $res, logging out...").trace
      // End of critial section
      _ <- semaphore.releaseN(requiredPermits)
    } yield res

  def demoWeightedSemaphore: IO[Unit] =
    for {
      // If use 2 in semaphore below, user3Fiber never start, becouse not enough permits 2/3 :(
      semaphore  <- Semaphore[IO](3)
      user1Fiber <- weightedLogin(1, 1, semaphore).start
      user2Fiber <- weightedLogin(2, 2, semaphore).start
      user3Fiber <- weightedLogin(3, 3, semaphore).start
      _          <- user1Fiber.join
      _          <- user2Fiber.join
      _          <- user3Fiber.join
    } yield ()

  // Exercise:
  // 1. Find out if there's something wrong with this code - tasks not going one by one
  // 2. Why? -
  // 3. Fix it
  // Semaphore with 1 permit == Mutex
  val mutex = Semaphore[IO](1)

  val users = (1 to 10).toList.parTraverse { id =>
    for {
      semaphore <- mutex
      _         <- IO(s"[Session $id] waiting to log in...").trace
      _         <- semaphore.acquire
      // Critical section
      _   <- IO(s"[Session $id] logged in, working...").trace
      res <- doWorkWhileLoggedIn
      _   <- IO(s"[Session $id] done: $res, logging out...").trace
      // End of critial section
      _ <- semaphore.release
    } yield res
  }

  val usersFixed = mutex.flatMap { semaphore =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[Session $id] waiting to log in...").trace
        _ <- semaphore.acquire
        // Critical section
        _   <- IO(s"[Session $id] logged in, working...").trace
        res <- doWorkWhileLoggedIn
        _   <- IO(s"[Session $id] done: $res, logging out...").trace
        // End of critial section
        _ <- semaphore.release
      } yield res
    }
  }

  override def run: IO[Unit] = usersFixed.trace.void
}
