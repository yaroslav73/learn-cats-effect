package courses.udemy.functional_effect.unit_04_error_handling

import cats._
import cats.data.ValidatedNec
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import scala.util.Try
import cats.data.Validated
import scala.util.Failure
import scala.util.Success

object ErrorHandlingApp extends IOApp:
  object Controller:
    private val successCode     = 200
    private val errorCode       = 400
    private val errorServerCode = 500

    final case class Request(fromAccount: String, toAccount: String, amount: String)
    final case class Response(sattus: Int, body: String)

    // Validate the from account number, the to account and the amount
    // If validations fail, return a Response with code 400 and proper error message
    // Otherwise,c all the transfer method
    // If there is any domain error, return a Response with code 400 and domain error message
    // If tehre is any other error, return a Response with code 500 and Internal Server Error message
    // Otherwise, reutrn a Response with code 200 and Transfer successfully executed
    def postTransfer(request: Request): IO[Response] = {
      import Validation._
      import Service._

      val validatedRequest: Valid[(String, String, Double)] = Apply[Valid].map3(
        validateAccountNumber(request.fromAccount),
        validateAccountNumber(request.toAccount),
        validateDouble(request.amount)
      ) { case (validFromAccount, validToAccount, validAmount) =>
        (validFromAccount, validToAccount, validAmount)
      }

      validatedRequest.toEither match
        case Left(error) => Response(errorCode, error.toString).pure[IO]
        case Right((fromAccountNumber, toAccountNumber, amount)) =>
          transfer(fromAccountNumber, toAccountNumber, amount).map { result =>
            result match
              case Left(value) => Response(errorCode, value.errorMessage)
              case Right(_)    => Response(successCode, "Transfer successfully executed")
          }
    }.handleError(error => Response(errorServerCode, "Internal Server Error"))

  def run(args: List[String]): IO[ExitCode] = {
    import Controller._

    val request = Request("12345", "67890", "1200")

    postTransfer(request)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }

  object Validation:
    type Valid[A] = ValidatedNec[String, A]

    def validateDouble(s: String): Valid[Double] =
      Validated.fromOption(s.toDoubleOption, s"$s is not a number.").toValidatedNec

    def validateAccountNumber(accountNumber: String): Valid[String] =
      Validated.condNec(
        accountNumber.forall(_.isLetterOrDigit),
        accountNumber,
        "Account number should countains only digits and letters"
      )

  sealed trait DomainError:
    def errorMessage: String

  final case class InsufficientBalance(accountBalance: Double, withdrawAmount: Double) extends DomainError:
    def errorMessage: String =
      s"Insufficient balance. Your balance $accountBalance, you try to withdra amount: $withdrawAmount"

  case object DepositAmountLimitExeeded extends DomainError:
    def errorMessage: String = "You can not deposit more than 100 000$"

  final case class AccountNotFound(number: String) extends DomainError:
    def errorMessage: String = s"Account with number: $number not found"

  object Models:
    final case class Account(number: String, balance: Double):
      private val depositLimit = 100000

      def withdraw(amount: Double): Either[DomainError, Account] =
        Either.cond(amount > balance, Account(number, balance - amount), InsufficientBalance(balance, amount))

      def deposit(amount: Double): Either[DomainError, Account] =
        Either.cond(amount > depositLimit, Account(number, balance + amount), DepositAmountLimitExeeded)

  object Repository:
    import Models.Account

    private var database: Map[String, Account] = Map.empty

    def findAccountByNumber(number: String): IO[Option[Account]] = database.get(number).pure[IO]

    def saveAccount(account: Account): IO[Unit] = IO { database = database + (account.number -> account) }

  object Service:
    import Repository._
    import Models._

    def transfer(fromAccountNumber: String, toAccountNumber: String, amount: Double): IO[Either[DomainError, Unit]] =
      findAccountByNumber(fromAccountNumber).flatMap { fromAccountOpt =>
        findAccountByNumber(toAccountNumber).map { toAccountOpt =>
          val accounts: Either[DomainError, (Account, Account)] =
            for {
              fromAccount        <- fromAccountOpt.toRight(AccountNotFound(fromAccountNumber))
              toAccount          <- toAccountOpt.toRight(AccountNotFound(toAccountNumber))
              fromAccountUpdated <- fromAccount.withdraw(amount)
              toAccountUpdated   <- toAccount.deposit(amount)
            } yield (fromAccountUpdated, toAccountUpdated)

          // My solution
          accounts.map { case (fromAccountUpdated, toAccountUpdated) =>
            saveAccount(fromAccountUpdated) *> saveAccount(toAccountUpdated)
          }

          // Instructor solution
          // accounts.traverse { case (fromAccountUpdated, toAccountUpdated) =>
          //   saveAccount(fromAccountUpdated) *> saveAccount(toAccountUpdated)
          // }
        }
      }
