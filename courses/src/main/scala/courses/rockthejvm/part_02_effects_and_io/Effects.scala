package courses.rockthejvm.part_02_effects_and_io

import scala.concurrent.Future

object Effects extends App {
  // Referential transparency = can replace an expression with it's values
  // as many times as we want without changing behavior.

  // Example: print to the console
  val printOne: Unit = println("Effect")
  val printTwo: Unit = println()

  println(printOne == printTwo) // it's true, but not the same

  // Example: change a variable
  var zero = 0
  val one: Unit = zero += 1
  val two: Unit = ()

  println(one == two) // also true, but not the same

  // Side effects are inevitable for useful programs

  // Effect types
  // Properties:
  // - type signature describes the kind of calculation that will be performed
  // - type signature describes the value that will be calculated
  // - when side effects are needed, effect construction is separated from effect execution

  // Example: Option (possibly absent value)
  // - type signature describe the kind of calculation that will be performed - a possibly absent value
  // - type signature describe the value that will be calculated - computes a value of type A, if it exists
  // - effect construction is separated from effect execution - does not produce any side effect,
  //   so side effects are not needed
  // Option is an effect type
  val maybeInt: Option[Int] = Option(13)

  // Example: Future
  // - type signature describe the kind of calculation that will be performed - describes an asynchronous computation
  // - type signature describe the value that will be calculated - computes a value of type A, if it successful
  // - effect construction is separated from effect execution - side effect is required (allocation/scheduling a thread),
  //   but Future does not separate creating effect from it's execution.
  //   When we create Future it's started execution in separate thread
  // Future is NOT an effect type
  import scala.concurrent.ExecutionContext.Implicits.global
  val futureInt: Future[Int] = Future(42)

  // Example: custom IO
  // - type signature describe the kind of calculation that will be performed - describe any computation
  //   that might produce side effects
  // - type signature describe the value that will be calculated - computes a value of type A, if it successful
  // - effect construction is separated from effect execution - side effect is required for evaluation of () => A
  //   and the creation of _IO does not produce the side effects on construction, so creating side effects
  //   separated from execution it
  final case class _IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): _IO[B] = _IO(() => f(unsafeRun()))

    // What happened if I do not execute second time, like this f(unsafeRun())?
    // Yes, result type is the same, but behavior is different.
    // f(unsafeRun()) is execute () => A, and return new _IO build by f
    // In case _IO(() => f(unsafeRun()).unsafeRun()) we construct new _IO, but does not execute it.
    def flatMap[B](f: A => _IO[B]): _IO[B] = _IO(() => f(unsafeRun()).unsafeRun())
  }

  var a = 0
  // Until call .unsafeRun() nothing happens and a still 0
  _IO(() => { a += 1 ; println(s"1. a is $a") }).flatMap(_ => _IO(() => { a += 1 ; println(s"2. a is $a") }))
  println(a)

  val ioInt: _IO[Int] = _IO { () =>
    println("Something execution...")
    10
  }

  // Exercise:
  // 1. An IO which returns the current time of the system
  // 2. An IO which measure the duration of a computation
  // 3. An IO which prints something to the console
  // 4. An IO which reads something to the console

  def time: _IO[Long] = _IO(() => System.currentTimeMillis)

  def measure[A](computation: _IO[A]): _IO[Long] =
    for {
      start <- time
      _ <- computation
      end <- time
    } yield end - start

  // Decompile the measure:
  // time.flatMap(start => computation.flatMap(_ => time.map(end => end - start))):
  // time.map(end => end - start) == _IO(() => time.unsafeRun() - start) ==
  //                                 _IO(() => _IO(() => System.currentTimeMillis).unsafeRun() - start) ==
  //                                 _IO(() => System.currentTimeMillis - start)
  // computation.flatMap(_ => _IO(() => System.currentTimeMillis - start)) == computation.flatMap(lambda) ==
  //                                                                          _IO(() => computation.unsafeRun()) ==
  //                                                                          _IO(() => computed value)
  // time.flatMap(start => computation.flatMap(_ => time.map(end => end - start))) ==
  //    time.flatMap(start => lambda) == _IO(() => time.unsafeRun()) ==
  //                                      _IO(() => _IO(() => System.currentTimeMillis).unsafeRun()) ==
  //                                      _IO(() => System.currentTimeMillis)
  // And after unsafeRun() it's looks like:
  // _IO(() => System.currentTimeMillis).flatMap { start =>
  //    _IO(() => computed value).flatMap { _ =>
  //        _IO(() => System.currentTimeMillis).map { end =>
  //            end - start
  //        }
  //    }
  // }

  def testMeasure(): Unit = {
    val test = measure(_IO(() => Thread.sleep(1000)))
    println(s"Computation time: ${test.unsafeRun()}")
  }
  testMeasure()

  def printIO[A](value: A): _IO[Unit] = _IO(() => println(value))

  def readIO: _IO[String] = _IO(() => scala.io.StdIn.readLine())

  val program: _IO[Unit] =
    for {
      _ <- printIO("Enter your name:")
      name <- readIO
      _ <- printIO("Enter your age:")
      age <- readIO
      _ <- printIO(s"Hello, $name, your age is: ${age.toInt}")
    } yield ()

  program.unsafeRun()
}
