package ch09

import cats.{Eval, MonadError}
import cats.data.{Reader, Writer, State}
import cats.syntax.applicative.catsSyntaxApplicativeId  // pure
import cats.syntax.writer.catsSyntaxWriterId  // tell
import cats.syntax.apply.catsSyntaxApplyOps  // *>

object Lib:
  /*
    9.5.4 Exercise: Abstracting
    Implement a method validateAdult with the following signature
   */
  // type MonadThrow[F[_]] = MonadError[F, Throwable]
  // def validateAdult[F[_] : MonadThrow as me](age: Int): F[Int]

  // def validateAdult[F[_] : ([G[_]] =>> MonadError[G, Throwable]) as me](age: Int): F[Int] =
  def validateAdult[F[_]](age: Int)(using me: MonadError[F, Throwable]): F[Int] =
    me.ensure(me.pure(age))(IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)

  /*
    9.6.5 Exercise: Safer Folding using Eval
    The naive implementation of foldRight below is not stack safe. Make it so using Eval.
   */
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    def foldR(xs: List[A]): Eval[B] =
      xs match
        case head :: tail => Eval.defer(foldR(tail).map(fn(head, _)))
        case Nil          => Eval.now(acc)

    foldR(as).value

  /*
    9.7.3 Exercise: Show Your Working

    Rewrite factorial so it captures the log messages in a Writer.
    Demonstrate that this allows us to reliably separate the logs for concurrent computations.
   */
  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for
      ans <-
        if (n == 0)
        then 1.pure[Logged]
        else slowly(factorial(n - 1).map(_ * n))
      // The log in a Writer is preserved when we map or flatMap over it.
      _ <- Vector(s"fact $n $ans").tell
    yield ans

  /*
    9.8.3 Exercise: Hacking on Readers

    The classic use of Readers is to build programs that accept a configuration as a parameter.
    Let's ground this with a complete example of a simple login system.
    Our configuration will consist of two databases: a list of valid users and a list of their passwords.

    Start by creating a type alias DbReader for a Reader that consumes a Db as input.

    Now create methods that generate DbReaders to look up the username for an Int user ID,
    and look up the password for a String username.

    Finally create a checkLogin method to check the password for a given user ID.
   */
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for
      username <- findUsername(userId)
      passwordOk <- username
        .map(checkPassword(_, password))
        .getOrElse(false.pure[DbReader])
    yield passwordOk

  /*
    9.9.3 Exercise: Post-Order Calculator
    Let's write an interpreter for post-order expressions.
    We can parse each symbol into a State instance representing
    a transformation on the stack and an intermediate result.

    Start by writing a function evalOne that parses a single symbol into an instance of State.
    If the stack is in the wrong configuration, it's OK to throw an exception.
   */
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int]:
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)

      case _ => sys.error("Fail!")

  /*
  Generalise this example by writing an evalAll method that computes the result of a List[String].
  Use evalOne to process each symbol, and thread the resulting State monads together using flatMap.
   */
  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (s, x) =>
      // We discard the value, but must use the previous
      // state for the next computation.
      // Simply invoking evalOne will create a new state.
      s *> evalOne(x)
    }

  /*
  Complete the exercise by implementing an evalInput function that splits an input String into symbols,
  calls evalAll, and runs the result with an initial stack.
   */
  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
