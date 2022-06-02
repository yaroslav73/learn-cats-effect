package courses.exercises

import cats.effect.IO
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class IntroExercisesTests extends AnyWordSpec with Matchers {
  import courses.rockthejvm.part_02_effects_and_io.exercises.IntroExercises._
  import cats.effect.unsafe.implicits.global

  "takeLast should discard first effect" in {
    val mutableArray = collection.mutable.ArrayBuffer.empty[String]

    val firstIO = IO {
      mutableArray += "first effect"
      "first effect"
    }

    val secondIO = IO {
      mutableArray += "second effect"
      13
    }

    takeLast(firstIO, secondIO).unsafeRunSync() shouldBe 13
    mutableArray should have size 2
  }

  "takeFirst should discard last effect" in {
    val mutableArray = collection.mutable.ArrayBuffer.empty[String]

    val firstIO = IO {
      mutableArray += "first effect"
      "first effect"
    }

    val secondIO = IO {
      mutableArray += "second effect"
      13
    }

    takeFirst(firstIO, secondIO).unsafeRunSync() shouldBe "first effect"
    mutableArray should have size 2
  }

  "convert should return new IO with new Type" in {
    val mutableArray = collection.mutable.ArrayBuffer.empty[String]

    val io = IO {
      mutableArray += "int"
      13
    }

    convert(io, "Hello!").unsafeRunSync() shouldBe "Hello!"
    mutableArray should have size 1
  }

  "unit should run IO and return Unit" in {
    val mutableArray = collection.mutable.ArrayBuffer.empty[String]

    val io = IO {
      mutableArray += "int"
      13
    }

    unit(io).unsafeRunSync() shouldBe ()
    mutableArray should have size 1
  }

  "sumIO should calculate sum of numbers without StackOverflowError" in {
    assertThrows[StackOverflowError] {
      sum(20000)
    }

    sumIO(20000).unsafeRunSync() shouldBe 200010000
  }

  "fibonacci should calculate fibonacci number without StackOverflowError" in {
    fibonacci(21).unsafeRunSync() shouldBe 10946
  }
}
