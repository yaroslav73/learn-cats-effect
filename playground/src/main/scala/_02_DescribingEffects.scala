import cats.effect.IO

import cats.effect.unsafe.implicits.global

object _02_DescribingEffects:
  // IO is a potent abstraction that can efficiently describe multiple kinds of effects:
  // 1. Pure Values — IO.pure & IO.unit
  // Lift pure values into IO, yielding IO values that are "already evaluated"
  val pureAndApply = IO.pure(13).flatMap(n => IO(println(s"The number is: $n")))

  val unit = IO.unit

  // 2. Synchronous Effects — IO.apply
  // Note the given parameter is passed ''by name'', its execution being "suspended" in the IO context.
  def putStrLn(s: String) = IO(println(s))
  val readLn              = IO(scala.io.StdIn.readLine())

  val simpleConsoleApp =
    for {
      _ <- putStrLn("What is your name?")
      n <- readLn
      _ <- putStrLn(s"Hello $n!")
    } yield ()

  // 3. Asynchronous Effects — IO.async & IO.cancelable
  // 4. Deferred Execution — IO.defer (previously suspend)

  @main def runExamples02(): Unit =
    pureAndApply.unsafeRunSync()

    unit.flatMap(v => IO(println(v))).unsafeRunSync()

  @main def consoleExample02(): Unit =
    simpleConsoleApp.unsafeRunSync()
