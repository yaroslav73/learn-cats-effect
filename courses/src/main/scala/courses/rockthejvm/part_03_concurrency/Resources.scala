package courses.rockthejvm.part_03_concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import courses.rockthejvm.part_03_concurrency.exercises.ResourcesExercises.openFileScanner

import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // Use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String]  = IO(s"Open connection to: $url ...").debug
    def close: IO[String] = IO(s"Close connection to: $url ...").debug
  }

  val url = "hello.example.com"

  def wrongAsyncFetchUrl(url: String): IO[Unit] =
    for {
      fiber <- (Connection(url).open *> IO.sleep(Int.MaxValue.seconds)).start
      _     <- IO.sleep(1.second) *> fiber.cancel
    } yield ()
  // Problem with code above - leaking resource, we open connection but not close it

  def correctAsyncFetchUrl(url: String): IO[Unit] =
    for {
      connection <- IO(Connection(url))
      fiber      <- (connection.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(connection.close.void).start
      _          <- IO.sleep(1.second) *> fiber.cancel
    } yield ()

  // Not bad, but CE has the Bracket Pattern:
  // - IO.bracket(useResourceCallback)(releaseResourceCallback)
  // - bracket is equivalent to try-catch, but pure FP
  def bracketFetchUrl(url: String): IO[Unit] =
    for {
      fiber <- IO(Connection(url))
        .bracket(connection => connection.open *> IO.sleep(Int.MaxValue.seconds))(connection => connection.close.void)
        .start
      _ <- IO.sleep(1.second) *> fiber.cancel
    } yield ()

  // Resources
  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // Acquire a connection based on the file
        IO(Connection(scanner.nextLine)).bracket { connection =>
          connection.open >> IO.never
        }(connection => connection.close.void)
      }(scanner => IO("closing file...").debug >> IO(scanner.close()))
  // Nesting resource are tedious

  // Define how to create (Acquire) and close (Release) resource, but not use
  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(Connection(url)))(connection => connection.close.void)

  val resourceFetchUrl: IO[Unit] = for {
    fiber <- connectionResource.use(connection => connection.open >> IO.never).start
    _     <- IO.sleep(1.second) >> fiber.cancel
  } yield ()

  // Resource are equivalent to bracket
  val simpleResource                        = IO("some resources...")
  val usingResource: String => IO[String]   = s => IO(s"using a string: $s").debug
  val releasingResource: String => IO[Unit] = s => IO(s"finalizing a string: $s").debug.void

  val usingResourceWithBracket  = simpleResource.bracket(usingResource)(releasingResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releasingResource).use(usingResource)

  // Nested resources
  def connectionFromConfigResource(path: String): Resource[IO, Connection] =
    Resource
      .make(IO(s"Open file: $path...").debug >> openFileScanner(path))(scanner =>
        IO("closing file...").debug >> IO(scanner.close())
      )
      .flatMap { scanner =>
        Resource.make(IO(Connection(scanner.nextLine())))(connection => connection.close.void)
      }
  def connectionFromConfigResourceClean(path: String): Resource[IO, Connection] =
    for {
      scanner <- Resource.make(IO(s"Open file: $path...").debug >> openFileScanner(path))(scanner =>
        IO("closing file...").debug >> IO(scanner.close())
      )
      connection <- Resource.make(IO(Connection(scanner.nextLine())))(connection => connection.close.void)
    } yield connection

  val path: String   = "courses/src/main/resources/url.txt"
  val openConnection = connectionFromConfigResourceClean(path).use(connection => connection.open >> IO.never)

  val nestedResourceFetchUrl =
    for {
      fiber <- openConnection.start
      _     <- IO.sleep(2.seconds) >> IO("Cancelling...").debug >> fiber.cancel
    } yield ()

  // Finalizers for regular IOs
  val ioWithFinalizer = IO("Some resource...").debug.guarantee(IO("freeing resource...").debug.void)

  val ioWithFinalizerCase = IO("Some resource...").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(res => IO(s"Releasing resources: $res...").debug).void
    case Errored(e)    => IO(s"Nothing to release. We failed with: ${e.getMessage}").debug.void
    case Canceled()    => IO("Resource cancelled, nothing to release").debug.void
  }

  override def run: IO[Unit] =
    ioWithFinalizer.void
}
