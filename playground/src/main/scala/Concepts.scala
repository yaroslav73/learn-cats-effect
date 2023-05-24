import cats.effect.IO
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

// 2. Terminology
// 2.A. Asynchronous

// Synchronous effects are defined using apply (also delay, blocking, interruptible or interruptibleMany)
// and produce their results using return, or alternatively raise errors using throw.
val ioUnit = IO {
  Thread.sleep(500)
  println(s"[${Thread.currentThread().getName()}]: Hello")
}

// Asynchronous effects are defined using async (or async_) and produce their results using a callback,
// where a successful result is wrapped in Right while an error is wrapped in Left
val scheduler = Executors.newScheduledThreadPool(1)

val ioAsync = IO.async_[String] { callback =>
  scheduler.schedule(
    new Runnable {
      // def run = callback(Right(println(s"[${Thread.currentThread().getName()}]: Hello, Async!")))
      def run = callback(Right(s"[${Thread.currentThread().getName()}]: Hello, Async!"))
    },
    500,
    TimeUnit.MILLISECONDS
  )

// ()
}

// All fibers are sequences of effects.
// "Asynchronous" simply means "produces values or errors using a callback rather than return/throw".

@main def runSyncAndAsync: Unit =
  import cats.effect.unsafe.implicits._
  ioUnit.unsafeRunSync()
  ioAsync.map(println).unsafeRunSync()

// 2.B. Concurrent
// Concurrency is often conflated with asynchronous execution due to the fact that, in practice,
// the implementation of concurrency often relies upon some mechanism for asynchronous evaluation.

// Structured Concurrency
// Formally-speaking, structured concurrency is a form of control flow
// in which all concurrent operations must form a closed hierarchy.
