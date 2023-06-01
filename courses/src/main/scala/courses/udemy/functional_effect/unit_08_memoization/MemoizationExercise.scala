package courses.udemy.functional_effect.unit_08_memoization

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.traverse.toTraverseOps

import concurrent.duration.DurationInt

object MemoizationExercise extends IOApp:
  enum Currency:
    case Dollar
    case Euro

  final case class Balance(amount: Double, currency: Currency)

  // Pretent this is calling an API and takes some time
  def fetchDollarExchangeRate(currency: Currency): Double =
    Thread.sleep(5000)
    currency match
      case Currency.Dollar => 1.0
      case Currency.Euro   => 1.12

  def fetchDollarExchangeRateIO(currency: Currency): IO[Double] =
    IO.sleep(5000.millis) *> IO.println("Fetch exchange rate...") *> IO {
      currency match
        case Currency.Dollar => 1.0
        case Currency.Euro   => 1.12
    }

  lazy val euroExchangeRate: Double = fetchDollarExchangeRate(Currency.Euro)

  val euroExchangeRateMemoizeIO: IO[IO[Double]] = fetchDollarExchangeRateIO(Currency.Euro).memoize

  def getBalanceInDollars(balances: List[Balance]): List[Double] =
    balances.map { balance =>
      balance.currency match
        case Currency.Dollar => balance.amount
        case Currency.Euro   => balance.amount * euroExchangeRate
    }

  def getBalanceInDollarsIO(balances: List[Balance]): IO[List[Double]] =
    euroExchangeRateMemoizeIO.flatMap { euroExchangeRateIO =>
      balances.map { balance =>
        balance.currency match
          case Currency.Dollar => balance.amount.pure[IO]
          case Currency.Euro =>
            euroExchangeRateIO.map { _ * balance.amount }
      }.sequence
    }

    // TODO: Wrong. In this case we fetched exchange rate two times.
    // balances.map { balance =>
    //   balance.currency match
    //     case Currency.Dollar => balance.amount.pure[IO]
    //     case Currency.Euro =>
    //       euroExchangeRateIO.flatMap { euroExchangeRate =>
    //         euroExchangeRate.map { _ * balance.amount }
    //       }
    // }.sequence

  def run(args: List[String]): IO[ExitCode] =
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchnage rate once
    val dollarBalances = List(
      Balance(1.23, Currency.Dollar),
      Balance(12.3, Currency.Dollar),
      Balance(1233, Currency.Dollar),
      Balance(75.2, Currency.Dollar),
      Balance(13.7, Currency.Dollar)
    )
    val combineBalances = List(
      Balance(1.23, Currency.Dollar),
      Balance(12.3, Currency.Dollar),
      Balance(1233, Currency.Euro),
      Balance(75.2, Currency.Euro),
      Balance(13.7, Currency.Dollar)
    )

    val balances = combineBalances

    IO {
      if balances.forall(_.currency == Currency.Dollar) then balances
      else getBalanceInDollars(balances).map(amount => Balance(amount, Currency.Dollar))
    }
      .flatTap(IO.println)
      .as(ExitCode.Success)

    (
      if balances.forall(_.currency == Currency.Dollar) then balances.pure[IO]
      else getBalanceInDollarsIO(balances).map(amounts => amounts.map(amount => Balance(amount, Currency.Dollar)))
    )
      .flatTap(IO.println)
      .as(ExitCode.Success)
