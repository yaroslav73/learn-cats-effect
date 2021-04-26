package effects._01_side_effect_intro

object _03_EffectsComposition extends App {
  val hello = MyIO.putStr("Hello")
  val effects = MyIO.putStr("Effects")

  val result = for {
    _ <- hello
    _ <- effects
  } yield ()

  result.unsafeRun()
}
