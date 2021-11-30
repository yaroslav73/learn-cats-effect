package effects._01_side_effect_intro

object _03_EffectsComposition extends App {
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
    o <- one
    t <- two
    th <- three
    res = o - t - th
    _ <- MyIO.putStr(s"Result: $res")
  } yield ()).unsafeRun()
}
