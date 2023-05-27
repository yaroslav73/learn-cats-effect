package courses.udemy.functional_effect.section_04

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

object ErrorHandlingAppExercise extends IOApp:

  // Task:
  // User first argument of args as the filename to load (assume it is always provided)
  // Validate the filename, and output any problem via console
  // If loaded successfully, output the number of words in the file (user Service.countWords)
  // If a domain error orrucrs, communicate it to the user via the console
  // If a technical, non-fatal error occurs, output "Something went wrong" to the console
  // If a fatal error occurs, just re-raise it and let everything fails

  def run(args: List[String]): IO[ExitCode] = ???
