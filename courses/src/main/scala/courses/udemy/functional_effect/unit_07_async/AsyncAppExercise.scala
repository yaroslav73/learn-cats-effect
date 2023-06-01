package courses.udemy.functional_effect.unit_07_async

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

object AsyncAppExercise extends IOApp:
  final case class User(id: Long, username: String)
  type Error = String

  def findUser(id: Long)(callback: Either[Error, User] => Unit): Unit =
    if (math.random() > 0.5) callback(Right(User(id, s"User_$id")))
    else callback(Left(s"User with id: $id does not exist"))

  def findUserIO(id: Long): IO[User] =
    IO.async_ { callback =>
      findUser(id) {
        case Right(user) => callback(Right(user))
        case Left(error) => callback(Left(new Exception(error)))
      }
    }

  def run(args: List[String]): IO[ExitCode] =
    findUserIO(73)
      .flatTap(IO.println)
      .as(ExitCode.Success)
