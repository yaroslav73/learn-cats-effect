package courses.rockthejvm.part_04_concurrent_coordination

import cats.effect.{IO, IOApp, Ref}

object Refs extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // Ref is a purely functional atomic reference
  val atomicNumber: IO[Ref[IO, Int]]        = Ref[IO].of(13)
  val anotherAtomicNumber: IO[Ref[IO, Int]] = IO.ref(13)

  // Modifying ref is an effect
  val increase: IO[Unit] = atomicNumber.flatMap { ref =>
    ref.set(14) // .update(_ + 1) // thread safe
  }

  // Obtain value
  val number: IO[Int] = atomicNumber.flatMap { ref =>
    ref.get // thread safe
  }

  // Gets the old value and set new one
  val getAndSetNumber: IO[Int] = atomicNumber.flatMap { ref =>
    ref.getAndSet(21)
  }

  // Update with a function
  val updateNumber: IO[Unit] = atomicNumber.flatMap { ref =>
    ref.update(_ * 3)
  }

  // Can also use getAndUpdate to get the OLD value
  val updateAndGetNumber: IO[Int] = atomicNumber.flatMap { ref =>
    ref.updateAndGet(_ * 3) // Get the new value
  }

  // Modifying with a function that returning different type
  val modifyNumber: IO[String] = atomicNumber.flatMap { ref =>
    ref.modify(n => (n * 3, s"Current value is: $n"))
  }

  // Why we need Ref?
  // Concurrent + thread safe reads/writes over shared values in a purely function way

  def demoConcurrentWorkImpure: IO[Unit] = {
    import cats.syntax.parallel._

    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _        <- IO(s"Counting words for '$workload': $wordCount").trace
        newCount <- IO(count + wordCount)
        _        <- IO(s"New total: $newCount").trace
        _        <- IO(count += wordCount)
      } yield ()
    }

    List("I love Cats Effect", "This Ref things is useless", "Daniel writes a lot of code")
      .map(task)
      .parSequence
      .void
  }

  // Drawbacks:
  // - hard to read/debug
  // - mix pure/impure code
  // - NOT THREAD SAFE

  def demoConcurrentWorkPure: IO[Unit] = {
    import cats.syntax.parallel._
    import cats.implicits._

    def task(workload: String, ref: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _        <- IO(s"Counting words for '$workload': $wordCount").trace
        current  <- ref.get
        _        <- IO(s"Current value: $current").trace // 0, because execute on different threads.
        newCount <- ref.updateAndGet(_ + wordCount)
        _        <- IO(s"New total: $newCount").trace
      } yield ()
    }

    for {
      count <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This Ref things is useless", "Daniel writes a lot of code")
        .map(task(_, count))
        .parSequence // .sequence // for non parallel execution
        .void
    } yield ()
  }

  override def run: IO[Unit] =
    atomicNumber.flatMap(ref => ref.get).trace.void >>
      demoConcurrentWorkPure
}
