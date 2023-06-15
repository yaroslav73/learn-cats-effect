package courses.udemy.functional_effect.unit_12_queue

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.parallel.catsSyntaxParallelTraverse1
import cats.syntax.parallel.catsSyntaxParallelSequence_
import cats.syntax.foldable.toFoldableOps
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
      dir   <- IO.blocking(new File(directory))
      files <- IO.blocking(dir.listFiles.toList.filter(f => f.isFile && f.getName.endsWith(".jpg")))
      images <- files.parTraverse(f =>
        IO.println(s"Loading image: ${f.getName}") *> IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f)))
      )
    } yield images

  // TODO: Take a processed image from and save it into corresponding file
  def imageSaver(processedImageQueue: Queue[IO, ImageInfo]): IO[Unit] =
    processedImageQueue.take.flatMap { imageInfo =>
      IO.println(s"Save ${imageInfo.filepath}") *>
        saveImage(imageInfo) *>
        IO.println(s"Saved ${imageInfo.filepath}")
    }.foreverM

  // TODO: Take a raw image from the queue, process it and put it in the processed queue
  def imageProcessor(
    rawImageQueue: Queue[IO, ImageInfo],
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    (for {
      rawImage <- rawImageQueue.take
      _        <- IO.println(s"Processing ${rawImage.filepath}")
      processedImage = processImage(rawImage)
      _ <- processedImageQueue.offer(processedImage)
    } yield ()).foreverM

  // TODO: Load image from the directory and put them into the queue
  def imageLoader(
    sourceDirectory: String,
    rawImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    IO.println(s"Start loading images from $sourceDirectory") *>
      loadImages(sourceDirectory).flatMap { images =>
        IO.println(s"Load ${images.size} images from the $sourceDirectory and put them into the raw image queue") *>
          images.traverse_(image => rawImageQueue.offer(image))
      }

  // TODO: Create the image loaders, savers and processors and get them all running
  // How it should works:
  //  1. Load file from source directories and put them into raw image queue (imageLoader)
  //  2. Process images - take from raw image queue -> process -> put it into the processed image queue (imageProcessor)
  //  3. Take from processed image queue and save proccess image into the source direrectories with updated name (imageSaver)
  def program(
    sourceDirectories: List[String],
    noProcessors: Int,
    noSavers: Int
  ): IO[Unit] =
    for {
      rawImageQueue       <- Queue.unbounded[IO, ImageInfo]
      processedImageQueue <- Queue.unbounded[IO, ImageInfo]
      loaders    = sourceDirectories.map(directory => imageLoader(directory, rawImageQueue))
      processors = List.range(0, noProcessors).map(_ => imageProcessor(rawImageQueue, processedImageQueue))
      savers     = List.range(0, noSavers).map(_ => imageSaver(processedImageQueue))
      _ <- (loaders ++ processors ++ savers).parSequence_
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    val directories = List("courses/src/main/resources/test1", "courses/src/main/resources/test2")
    program(directories, 3, 3).timeoutTo(20.seconds, IO.unit).as(ExitCode.Success)
