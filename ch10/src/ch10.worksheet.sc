import cats.data.{EitherT, OptionT, Writer}
import cats.syntax.applicative.catsSyntaxApplicativeId // pure
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

// 10.2 A Transformative Example
/*
we build ListOption from the inside out: we pass List, the type of the outer monad,
as a parameter to OptionT, the transformer for the inner monad.
 */
type ListOption[A] = OptionT[List, A]

// We can create instances of ListOption using the OptionT constructor, or more conveniently using pure
val result1: ListOption[Int] = OptionT(List(Option(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

// 10.3.2 Building Monad Stacks

// Alias Either to a type constructor with one parameter
type ErrorOr[A] = Either[String, A]

// Build our final monad stack using OptionT
type ErrorOrOption[A] = OptionT[ErrorOr, A]

val a = 10.pure[ErrorOrOption]
val b = 32.pure[ErrorOrOption]

val c = a.flatMap(x => b.map(y => x + y))

type FutureEither[A] = EitherT[Future, String, A]

type FutureEitherOption[A] = OptionT[FutureEither, A]

val futureEitherOr: FutureEitherOption[Int] =
  for
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  yield a + b

// 10.3.3 Constructing and Unpacking Instances
val intermediate = futureEitherOr.value

val stack = intermediate.value

Await.result(stack, 1.second)

// 10.3.5 Usage Patterns
type Logged[A] = Writer[List[String], A]

// Methods generally return untransformed stacks
def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None      => Writer(List(s"Failed on $str"), None)

// Consumers use monad transformers locally to simplify composition
def addAll(a: String, b: String, c: String): Logged[Option[Int]] =
  val result = for
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  yield a + b + c

  result.value

// This approach doesn't force OptionT on other users' code:
addAll("1", "2", "3")
addAll("1", "a", "3")
