package courses.udemy.functional_effect.unit_06_resource_safety

import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import cats.effect.Resource

object ResourceExample extends IOApp:
  def read(fis: FileInputStream): IO[Array[Byte]] =
    IO.println("reading...") *> IO.blocking {
      Iterator
        .continually(fis.read)
        .takeWhile(_ != -1)
        .map(_.toByte)
        .toArray
    }

  def write(bytes: Array[Byte], fos: FileOutputStream): IO[Unit] =
    IO.println("writing...") *> IO.blocking(fos.write(bytes))

  def encrypt(bytes: Array[Byte]): IO[Array[Byte]] =
    IO.println("encrypting...") *> bytes.map(byte => (byte + 1).toByte).pure[IO]

  def closeReader(ac: AutoCloseable): IO[Unit] =
    IO.println("closing reader...") *> IO.blocking(ac.close)

  def closeWriter(ac: AutoCloseable): IO[Unit] =
    IO.println("closing writer...") *> IO.blocking(ac.close)

  def encryptFileV1(source: File, destination: File): IO[Unit] =
    val acquireReader = IO.println("Acquiring reader...") *> IO.blocking(new FileInputStream(source))
    val acquireWriter = IO.println("Acquiring writer...") *> IO.blocking(new FileOutputStream(destination))

    acquireReader.bracket { reader =>
      acquireWriter.bracket { writer =>
        read(reader).flatMap(encrypt(_)).flatMap(encryptedBytes => write(encryptedBytes, writer))
      }(writer => closeWriter(writer))
    }(reader => closeReader(reader))

  def encryptFileV2(source: File, destination: File): IO[Unit] =
    val acquireReader = IO.println("Acquiring reader...") *> IO.blocking(new FileInputStream(source))
    val acquireWriter = IO.println("Acquiring writer...") *> IO.blocking(new FileOutputStream(destination))

    val readerResource = Resource.make(acquireReader)(closeReader)
    val writerResource = Resource.make(acquireWriter)(closeWriter)

    readerResource.use { reader =>
      writerResource.use { writer =>
        read(reader)
          .flatMap(_ => IO.raiseError(new Exception("Failed reading...")))
          .flatMap(encrypt(_))
          .flatMap(encryptedBytes => write(encryptedBytes, writer))
      }
    }

    // Or we can cobibe reader and writer resources and get tuple of it Resource[IO, (FileInputStream, FileOutputStream)]
    // val readerAndWriterResources = readerResource.flatMap(reader => writerResource.map(writer => (reader, writer)))
    //
    // readerAndWriterResources.use { case (reader, writer) =>
    //  read(reader).flatMap(encrypt(_)).flatMap(encryptedBytes => write(encryptedBytes, writer))
    // }

  def encryptFileV3(source: File, destination: File): IO[Unit] =
    val acquireReader = IO.println("Acquiring reader...") *> IO.blocking(new FileInputStream(source))
    val acquireWriter = IO.println("Acquiring writer...") *> IO.blocking(new FileOutputStream(destination))

    val readerResource = Resource.fromAutoCloseable(acquireReader)
    val writerResource = Resource.fromAutoCloseable(acquireWriter)

    readerResource.use { reader =>
      writerResource.use { writer =>
        read(reader)
          .flatMap(encrypt(_))
          .flatMap(encryptedBytes => write(encryptedBytes, writer))
      }
    }

  def run(args: List[String]): IO[ExitCode] =
    val source      = new File("courses/src/main/resources/example.txt")
    val destination = new File("courses/src/main/resources/encrypted_example.txt")

    encryptFileV2(source, destination).as(ExitCode.Success)
