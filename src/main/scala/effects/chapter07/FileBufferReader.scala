package effects.chapter07

import cats.effect.{ExitCode, IO, IOApp, Resource}
import effects.debug._

import java.io.RandomAccessFile

class FileBufferReader private (in: RandomAccessFile) {
  def readBuffer(offset: Long): IO[(Array[Byte], Int)] = IO {
    in.seek(offset)

    val buffer = new Array[Byte](FileBufferReader.bufferSize)
    val length = in.read(buffer)

    (buffer, length)
  }

  private def close: IO[Unit] = IO(in.close())
}

object FileBufferReader {
  val bufferSize = 4096

  def makeResource(fileName: String): Resource[IO, FileBufferReader] =
    Resource.make {
      IO(new FileBufferReader(new RandomAccessFile(fileName, "r")))
    } { res => res.close }
}
