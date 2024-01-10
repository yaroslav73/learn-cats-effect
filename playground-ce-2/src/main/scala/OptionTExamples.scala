import cats.data.OptionT
import cats.effect.IO

//import cats.effect.unsafe.implicits.global
object OptionTExamples:
  @main def example01(): Unit =
    val optTNone = OptionT.none[IO, Int]
    val optT10   = OptionT.pure[IO](10)
    println(optT10)
    println(optTNone)

    val r1 = optT10.getOrElseF {
      IO(println("Hello"))
    }

    val r2 = optTNone.getOrElseF {
      IO(println("Hello"))
    }

    println(r1.unsafeRunSync())
    r2.unsafeRunSync()
