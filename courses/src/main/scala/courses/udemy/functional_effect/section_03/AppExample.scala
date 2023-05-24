package courses.udemy.functional_effect.section_03

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scala.io.StdIn
import cats.effect.IOApp
import cats.effect.ExitCode

// Possible outcome of running an IO computation:
// - never terminates
// - fails
// - return a value
object AppExample:
  object Console:
    def printLine(s: String): IO[Unit]       = IO(println(s))
    def readLine(prompt: String): IO[String] = IO(StdIn.readLine(prompt))
    def failed(s: String): IO[Unit]          = IO(throw new Exception("boom"))

@main def runExampleApp(): Unit =
  import AppExample.Console._

  readLine("Enter your name: ").flatMap { name =>
    printLine(s"Hello, $name!")
  }

  // The same as above
  val program =
    for {
      name <- readLine("Enter your name: ")
      _    <- printLine(s"Hello, $name!")
    } yield ()
  // program.unsafeRunSync()

  // If your value just a Unit, you can discard it using productR
  printLine("Hello").flatMap(_ => printLine("CE3!"))
  val program2 = printLine("Hello") *> printLine("CE3!")
  // program2.unsafeRunSync()

  val failedProgram =
    for {
      name <- readLine("Enther your name: ")
      _    <- failed(s"Hello, $name!")
    } yield ()
  // failedProgram.unsafeRunSync()

  val infiniteProgram = printLine("Thinking...").foreverM
  // infiniteProgram.unsafeRunSync()

// Instead of using .unsafeRunSync() use IOApp

object App extends IOApp:
  import AppExample.Console._

  // Read a line
  // Print a line
  // Repeat
  val echoForever: IO[Unit] =
    readLine("Enter something: ").flatMap(s => printLine(s"You enter: $s")).foreverM

  val echoForeverFor: IO[Unit] =
    (for {
      s <- readLine("Enter something: ")
      _ <- printLine(s"You enter: $s")
    } yield ()).foreverM

  val program =
    for {
      f <- readLine("Enter first word: ")
      s <- readLine("Enter second word: ")
      _ <- printLine(s"$f + $s = $f$s")
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    // echoForever.as(ExitCode.Success)
    echoForeverFor.as(ExitCode.Success)
