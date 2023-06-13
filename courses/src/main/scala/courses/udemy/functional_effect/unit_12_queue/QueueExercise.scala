package courses.udemy.functional_effect.unit_12_queue

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.parallel.catsSyntaxParallelTraverse1
import java.awt.image.ColorConvertOp
import java.awt.color.ColorSpace
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import concurrent.duration.DurationInt

object QueueExercise extends IOApp:
  final case class ImageInfo(filepath: String, image: BufferedImage)

  def processImage(imageInfo: ImageInfo): ImageInfo =
    val colorOp        = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
    val processedImage = colorOp.filter(imageInfo.image, imageInfo.image)
    imageInfo.copy(image = processedImage)

  def saveImage(imageInfo: ImageInfo): IO[Unit] =
    IO.blocking {
      val filepath    = imageInfo.filepath
      val newFilePath = s"${filepath.substring(0, filepath.length - 4)}_processed.jpg"
      ImageIO.write(imageInfo.image, "jpg", new File(newFilePath))
    }.void

  // TODO: Add Either[Error, List[ImageInfo]]
  def loadImages(directory: String): IO[List[ImageInfo]] =
    for {
      dir    <- IO.blocking(new File(directory))
      files  <- IO.blocking(dir.listFiles.toList.filter(f => f.isFile && f.getName.endsWith(".jpg")))
      images <- files.parTraverse(f => IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f))))
    } yield images

  // TODO: Take a processed image from and save it into corresponding file
  def imageSaver(processedImageQueue: Queue[IO, ImageInfo]): IO[Unit] =
    ???

  // TODO: Take a raw image from the queue, process it and put it in the processed queue
  def imageProcessor(
    rawImageQueue: Queue[IO, ImageInfo],
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    ???

  // TODO: Load image from the directory and put them into the queue
  def imageLoader(
    sourceDirectory: String,
    rawImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    ???

  // TODO: Create the image loaders, savers and processors and get them all running
  def program(
    sourceDirectories: List[String],
    noProcessors: Int,
    noSavers: Int
  ): IO[Unit] =
    ???

  def run(args: List[String]): IO[ExitCode] =
    val directories = List("test1", "test2")
    program(directories, 3, 3).timeoutTo(10.seconds, IO.unit).as(ExitCode.Success)
