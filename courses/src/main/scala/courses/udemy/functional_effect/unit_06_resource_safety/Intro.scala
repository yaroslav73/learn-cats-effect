package courses.udemy.functional_effect.unit_06_resource_safety

import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.io.File
import java.io.FileWriter
import java.net.URI

object Intro extends IOApp:
  trait RowEncoder[A]:
    def encode(a: A): String

  final case class Person(name: String, age: Int)
  object Person:
    given RowEncoder[Person] = new RowEncoder[Person] {
      def encode(p: Person): String = s"${p.name}, ${p.age}"
    }

  def writeAll[A](xs: List[A], file: File)(using encoder: RowEncoder[A]): IO[Unit] =
    IO {
      val fw = new FileWriter(file)
      xs.foreach(s => fw.write(encoder.encode(s) + '\n')) // TODO: What if error happened here? When we write to file?
      fw.close()
    }

  def writeAllLectureExample[A](xs: List[A], file: File)(using encoder: RowEncoder[A]): IO[Unit] =
    for {
      fw <- IO.blocking(new FileWriter(file))
      content = xs.map(encoder.encode(_)).mkString("\n")
      _ <- IO.blocking(fw.write(content)) *> IO.raiseError(new Exception("writing failed..."))
      _ <- IO.println("Content wrote...")
      _ <- IO.blocking(fw.flush())
      _ <- IO.println("Writer flushed...")
      _ <- IO.blocking(fw.close())
      _ <- IO.println("Writer closed...")
    } yield ()

  def writeAllWithBracket[A](xs: List[A], file: File)(using encoder: RowEncoder[A]): IO[Unit] =
    IO(new FileWriter(file))
      .bracket { fw =>
        IO(xs.foreach(s => fw.write(encoder.encode(s) + '\n'))) *> IO(fw.flush())
      } { _ => IO.unit }

  def writeAllInstructorExample[A](xs: List[A], file: File)(using encoder: RowEncoder[A]): IO[Unit] =
    val content = xs.map(encoder.encode(_)).mkString("\n")

    def use(fw: FileWriter): IO[Unit] =
      IO.println("Writing content...") *>
        IO.blocking(fw.write(content)) *>
        IO.raiseError(new Exception("Failed writing...")) *>
        IO.println("Flushing resources...") *>
        IO.blocking(fw.flush())

    def release(fw: FileWriter): IO[Unit] =
      IO.println("Closing resources...") *>
        IO.blocking(fw.close())

    IO.blocking(new FileWriter(file)).bracket(use)(release)

  def run(args: List[String]): IO[ExitCode] =
    val persons = List(Person("Marie", 31), Person("Yaroslav", 30))

    writeAllWithBracket(persons, new File("courses/src/main/resources/06_intro.txt")).as(ExitCode.Success)
