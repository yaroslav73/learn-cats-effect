package courses.udemy.functional_effect.unit_08_memoization

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

object MemoizationExercise extends IOApp:
  enum Currency:
    case Dollar
    case Euro

  final case class Balance(amount: Double, currency: Currency)

  // Pretent this is calling an API and takes some time
  def fetchDollarExchangeRate(currency: Currency): Double =
    Thread.sleep(2000)
    currency match
      case Currency.Dollar => 1.0
      case Currency.Euro   => 1.12

  lazy val euroExchangeRate: Double = fetchDollarExchangeRate(Currency.Euro)

  def getbalanceInDollars(balances: List[Balance]): List[Double] =
    balances.map { balance =>
      balance.currency match
        case Currency.Dollar => balance.amount
        case Currency.Euro   => balance.amount * euroExchangeRate

    }
  def run(args: List[String]): IO[ExitCode] =
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchnage rate once
    ???
