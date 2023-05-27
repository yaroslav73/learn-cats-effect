package courses.udemy.functional_effect.section_04

// import cats.implicits._
import cats.implicits.catsSyntaxTuple2Semigroupal
import cats.data.{Validated, ValidatedNec}
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.io.FileInputStream

object ErrorHandlingAppExercise extends IOApp:
  object Validations:
    type Valid[A] = ValidatedNec[String, A]

    def validateExtension(filename: String): Valid[String] =
      Validated.condNec(
        filename.endsWith(".txt"),
        filename,
        s"Ivalied extension of file: $filename. Expected .txt"
      )

    def validateLength(s: String, maxLength: Int): Valid[String] =
      Validated.condNec(
        s.length <= maxLength,
        s,
        s"String $s is over then $maxLength characters long"
      )

    def validateFileName(filename: String): Valid[String] =
      (validateExtension(filename), validateLength(filename, maxLength = 64)).mapN { case (_, s) => s }

  object Service:
    def countWords(content: String): Int =
      content.split("\\W+").length

    def loadFile(filename: String): IO[Either[DomainError, String]] = {
      def loadFileContent(filename: String): IO[Array[Byte]] = {
        IO.blocking(new FileInputStream(filename))
          .bracket { fis =>
            IO.blocking(
              Iterator
                .continually(fis.read)
                .takeWhile(_ != -1)
                .map(_.toByte)
                .toArray
            )
          } { fis => IO.blocking(fis.close) }
      }

      // TODO:
      // Implement a load file function that loads all the contents of a file into a String
      // If the file does not exist, capture that with the domain error FileNotFound
      ???
    }

  sealed trait DomainError:
    def errorMessage: String
  final case class FileNotFound(filename: String) extends DomainError:
    def errorMessage: String = s"$filename not found"

  // Task:
  // User first argument of args as the filename to load (assume it is always provided)
  // Validate the filename, and output any problem via console
  // If loaded successfully, output the number of words in the file (user Service.countWords)
  // If a domain error orrucrs, communicate it to the user via the console
  // If a technical, non-fatal error occurs, output "Something went wrong" to the console
  // If a fatal error occurs, just re-raise it and let everything fails

  def run(args: List[String]): IO[ExitCode] = ???
