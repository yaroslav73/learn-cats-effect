package effects._01_side_effect_intro

object _02_CheckSubstitutionForMyIO extends App {
  MyIO.putStr("Hello, effects!").unsafeRun()

  MyIO(() => println("Hello, effects!")).unsafeRun()

  println("Hello, effects!")
}
