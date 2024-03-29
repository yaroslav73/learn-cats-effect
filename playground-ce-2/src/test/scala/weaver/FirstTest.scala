package weaver

import weaver._
import cats.effect._

// Suites must be "objects" for them to be picked by the framework
object FirstTest extends SimpleIOSuite {

  val randomUUID = IO(java.util.UUID.randomUUID())

  // A test for side-effecting functions
  test("hello side-effects") {
    for {
      x <- randomUUID
      y <- randomUUID
    } yield expect(x != y)
  }
}
