package effects._01_side_effect_intro

object _01_Printing extends App {
  val hello = MyIO.putStr("Hello, effects!")

  hello.unsafeRun()
}
