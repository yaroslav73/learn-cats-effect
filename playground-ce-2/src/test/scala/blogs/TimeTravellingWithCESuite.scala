package blogs

import blogs.TimeTravellingWithCE.{RetryingService, Service, TimeoutService}
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits.*
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

class TimeTravellingWithCESuite extends AsyncFlatSpec with Matchers with ScalaFutures:
  import cats.effect.laws.util.TestContext

  implicit val testCtx: TestContext = TestContext()
  implicit val ioTimer: Timer[IO]   = testCtx.timer[IO]

  // Default configuration is set to 3s for timeouts
  // and 1s to wait between retries, with max retries = 5

  // Test cases:
  // Service should succeed when it finishes shortly before the timeout
  // Service should succeed after a few retries
  // Service should fail after exceeding retries caused by timeouts
  opaque type FailCount = Int
  object FailCount:
    def apply(n: Int): FailCount = n

  class FailingService(failCount: FailCount) extends Service:
    private var failCounter: FailCount = 0

    def run: IO[Unit] =
      if failCounter == failCount then IO(println("Success!"))
      else
        IO.pure({ failCounter = failCounter + 1 }) *>
          IO.raiseError(new Exception("Something went wrong..."))

    def accumulatedFailures: FailCount = failCounter

  class SlowService(responseTimes: List[FiniteDuration]) extends Service:
    private var attemptsCounter = 0

    def run: IO[Unit] =
      if attemptsCounter < responseTimes.length then
        ioTimer.sleep(responseTimes(attemptsCounter)) *>
          IO.pure({ attemptsCounter = attemptsCounter + 1 })
      else IO(println("Success!"))
    // sleeper(responseTimes, IO(println("Success!")))

    def attempts: Int = attemptsCounter

//    private def sleeper(responseTimes: List[FiniteDuration], response: IO[Unit]): IO[Unit] =
//      responseTimes match
//        case head :: tail =>
//          IO.pure {
//            println(s"before: $attemptsCounter, tail: $tail")
//            { attemptsCounter = attemptsCounter + 1 }
//            println(s"after: $attemptsCounter, tail: $tail")
//          } *>
//            ioTimer.sleep(head) *>
//            sleeper(tail, response)
//        case Nil => response

  it should "test example from documentation" in {
    // Can now simulate time
    val io = ioTimer.sleep(10.seconds) *> IO(1 + 1)
    val f  = io.unsafeToFuture()

    // This invariant holds true, because our IO is async
    f.value.isEmpty shouldBe true

    // Not yet completed, because this does not simulate time passing:
    testCtx.tick()
    f.value.isEmpty shouldBe true

    // Simulating time passing:
    testCtx.tick(10.seconds)
    f.futureValue shouldBe 2
  }

  final case class Service1(timer: Timer[IO]) {
    def call(): IO[Unit] = timer.sleep(10.seconds) *> IO(println("Service1 called"))
  }
  object Service1 {
    def make(timer: Timer[IO]) =
      // IO.sleep(2.seconds) *>
      IO(new Service1(timer))
  }

  final case class Service2() {
    def call(): IO[Unit] = IO(println("Service2 called"))
  }
  object Service2 {
    def make() = IO(new Service2())
  }

  it should "custome example" ignore {
    val io = for {
      s1 <- Service1.make(ioTimer)
      s2 <- Service2.make()
      _  <- ioTimer.sleep(10.seconds)
      _  <- s2.call()
      _  <- s1.call()
    } yield 2
    val f = io.unsafeToFuture()

    testCtx.tick(10.seconds)
    testCtx.tick(10.seconds)
    f.map(_ shouldBe 2)
  }

  it should "succeed when it finishes shortly before the timeout" in {
    val slowService = SlowService(List(2800.millis))
    val service     = new TimeoutService(slowService) // 3s

    val result = service.run.unsafeToFuture()
    testCtx.tick(TimeoutService.DefaultTimeout) // 3s forward in time

    result.futureValue shouldBe ()
  }

  it should "succeed after a few retries" in {
    val failingService = new FailingService(FailCount(2))
    val service        = new RetryingService(failingService)

    val result = service.run.unsafeToFuture()
    testCtx.tick(10.seconds)

    result.futureValue shouldBe ()
    failingService.accumulatedFailures shouldBe FailCount(2)
  }

  it should "fail due to exceeded retries on timeouts" in {
    val slowService      = new SlowService(responseTimes = List.fill(6)(4.seconds))
    val timingOutService = new TimeoutService(slowService)
    val retryingService  = new RetryingService(timingOutService)

    val result = retryingService.run.unsafeToFuture()
    testCtx.tick(30.seconds)

    result.failed.futureValue shouldBe a[TimeoutException]
    slowService.attempts shouldBe 5
  }
