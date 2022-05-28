package books.essential.effects.chapter02

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ConstructingIO extends App {
  // Constructing IO values
  // helloDelay has type IO[Unit] when it's executed
  // it print 'Hello, Cats Effect!' to the console and produce value ()
  val helloDelay: IO[Unit] = IO.delay(println("Hello, Cats Effect!"))
  val helloApply: IO[Unit] = IO(println("Hello, Cats Effect!"))

  val ohNo: IO[Int] = IO.delay(throw new RuntimeException("NOOOOOOOOOO!!!"))
  val ohNoo: IO[Int] = IO.raiseError(new RuntimeException("NOOOOOOOOOO!!!"))

  // Be careful! Do not perform any side effects when calling IO.pure,
  // because they will be eagerly evaluated and that will break substitution.
  val twelve: IO[Int] = IO.pure(12)

  implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  val future: Future[String] = Future("This is string")
  val ioFuture: IO[String] = IO.fromFuture(IO(future))

  ioFuture.map(println)

  ioFuture.map(println).unsafeRunSync()

  (for {
    _ <- ioFuture.map(println)
    _ <- ioFuture.map(println)
  } yield ()).unsafeRunSync()

  private val futureUnit: Future[Unit] = Future(
    println("this is string from future")
  )
  for {
    _ <- futureUnit
    _ <- futureUnit
  } yield ()

  val io: IO[Unit] = IO(println("this is string from io"))
  (for {
    _ <- io
    _ <- io
  } yield ()).unsafeRunSync()
}
