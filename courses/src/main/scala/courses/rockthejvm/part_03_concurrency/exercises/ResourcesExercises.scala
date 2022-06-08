package courses.rockthejvm.part_03_concurrency.exercises

import cats.effect.{IO, IOApp}
import cats.effect.kernel.Outcome

import java.io.{File, FileReader}
import java.util.Scanner

import scala.concurrent.duration.DurationInt

object ResourcesExercises extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // Exercise:
  // read the file with the bracket pattern
  // - open a scanner
  // - read the file line by line, every 100 milliseconds
  // - close the scanner
  // - if cancelled/throws error, close the scanner

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"Opening file $path").debug *>
      openFileScanner(path)
        .bracket(scanner => readLineByLine(scanner))(scanner =>
          IO(s"Closed scanner for file $path").debug *> IO(scanner.close())
        )

  val pathToFile = "courses/src/main/scala/courses/rockthejvm/part_03_concurrency/exercises/ResourcesExercises.scala"

  override def run: IO[Unit] = bracketReadFile(pathToFile)
}
