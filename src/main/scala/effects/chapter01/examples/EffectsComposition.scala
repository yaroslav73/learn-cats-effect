package effects.chapter01.examples

import effects.chapter01.MyIO

object EffectsComposition extends App {
  // Be careful! Composing effects must not execute them.
  val hello = MyIO.putStr("Hello")
  val effects = MyIO.putStr("Effects")

  val result = for {
    _ <- hello
    _ <- effects
  } yield ()

  result.unsafeRun()

  val one = MyIO(() => 1)
  val two = MyIO(() => 2)
  val three = MyIO(() => 3)

  (for {
    one <- one
    two <- two
    three <- three
    res = one + two + three
    _ <- MyIO.putStr(s"Result: $res")
  } yield ()).unsafeRun()
}
