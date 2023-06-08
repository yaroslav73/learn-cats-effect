package courses.udemy.functional_effect.unit_04_error_handling

// import cats.implicits._
import cats.implicits.catsSyntaxApplicativeId
import cats.implicits.catsSyntaxEitherId
import cats.implicits.catsSyntaxTuple2Semigroupal
import cats.implicits.catsSyntaxFoldableOps0
import cats.data.{Validated, ValidatedNec}
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.io.FileInputStream
import scala.util.control.NonFatal
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import java.io.FileNotFoundException

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
      (validateExtension(filename), validateLength(filename, maxLength = 128)).mapN { case (_, s) => s }

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
      loadFileContent(filename).attempt.map {
        case Right(value) => Right(value.map(_.toChar).mkString)
        case Left(_)      => Left(FileNotFound(filename))
      }

      // Instructor implementation
      loadFileContent(filename)
        .map { bytes =>
          new String(bytes).asRight[DomainError]
        }
        .handleErrorWith {
          case _: FileNotFoundException => FileNotFound(filename).asLeft[String].pure[IO]
          case t: Throwable             => IO.raiseError(t)
        }
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
  import Service._
  import Validations._

  def program(args: List[String]): IO[ExitCode] = (
    args.headOption match
      case None => IO.println("Error: Please, provide the file name as first argument of args.")
      case Some(filename) =>
        validateFileName(filename) match
          case Valid(filename) =>
            loadFile(filename).map {
              case Right(content) => println(s"Total words: ${Service.countWords(content)}")
              case Left(error)    => error.errorMessage
            }
          case Invalid(errors) => IO.println(s"Filename validation failed: ${errors.mkString_("\n")}")
  )
    .handleErrorWith {
      case NonFatal(_) => IO.println("Something went wrong")
      case e           => IO.raiseError(e)
    }
    .as(ExitCode.Success)

  def instructorSolution(filename: String): IO[ExitCode] =
    (
      validateFileName(filename) match
        case Valid(filename) =>
          loadFile(filename).flatMap {
            case Right(content) =>
              val wordsCount = countWords(content)
              IO.println(s"Words count: $wordsCount").as(ExitCode.Success)
            case Left(error) => IO.println(error.errorMessage).as(ExitCode.Error)
          }
        case Invalid(errors) =>
          IO.println(s"File validations failed:\n\t${errors.mkString_("\n\t")}").as(ExitCode.Error)
    ).handleErrorWith {
      case NonFatal(_)  => IO.println("Something went wrong").as(ExitCode.Error)
      case t: Throwable => IO.raiseError(t)
    }

  def run(args: List[String]): IO[ExitCode] =
    program(args)
