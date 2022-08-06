package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.MonadCancel
import cats.effect.Sync
import cats.Defer
import cats.syntax.applicative.*
import java.io.BufferedReader
import java.io.InputStreamReader

object PolymorphicSync extends IOApp.Simple {

  // "Suspend" computation in IO
  val delayIO = IO.delay {
    println("I'm an effect!")
    73
  }

  // On some specific thread pool for blocking computation
  val blockingIO = IO.blocking {
    println("Loading..")
    Thread.sleep(1000)
    72
  }

  // Synchronous computation
  trait _Sync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    // "Suspension" of a computation - will run on the CE thread pool
    def delay[A](thunk: => A): F[A]

    // Runs on the blocking thread pool
    def block[A](thunk: => A): F[A]

    // defer comes for free
    def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  val syncIO = Sync[IO] // given Sync[IO] in scope

  // Abilities: pure, map/flatMap, raiseError, uncancelable + delay / blocking

  // Same as IO.delay
  val delayIOSync = syncIO.delay {
    println("I'm an effect!")
    73
  }

  // Same as IO.blocking
  val blockingIOSync = syncIO.blocking {
    println("Loading..")
    Thread.sleep(1000)
    72
  }

  val deferredIO = IO.defer(delayIO)

  // Exercise: write a polymorphic console
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine: F[String]
  }

  object Console {
    def apply[F[_]: Sync]: F[Console[F]] = Sync[F].pure {
      new Console[F] {
        def println[A](a: A): F[Unit] = Predef.println(a).pure[F]
        def readLine: F[String]       = scala.io.StdIn.readLine().pure[F]
      }
    }

    import cats.syntax.functor.*
    def make[F[_]](using F: Sync[F]): F[Console[F]] =
      F.pure((System.in, System.out)).map { case (in, out) =>
        new Console[F] {
          def println[A](a: A): F[Unit] = F.blocking(out.println(a))
          def readLine: F[String] = {
            val bufferedReader = new BufferedReader(new InputStreamReader(in))
            F.blocking(bufferedReader.readLine())

            // There's a potential problem hanging one of the threads
            // from the blocking thread pool (oh - oh my! - one of the CE threads).
            //
            // Tehre's also F.interryptible(true/false) which attempts to block the thread
            // via thread iterrupts in case of cancel.
            // The flag tells whether you want the thread interrupt signals
            // to be sent repeatedly (true) or not (false).
          }
        }
      }
  }

  def consoleReader: IO[Unit] =
    for {
      console <- Console.make[IO]
      _       <- console.println("Hi, what is your name?")
      name    <- console.readLine
      _       <- console.println(s"Hi, $name! Nice to meet you!")
    } yield ()

  def run: IO[Unit] = consoleReader
}
