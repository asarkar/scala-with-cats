import cats.{Id, Eval, MonadError}
import cats.data.{Reader, Writer, State}
import cats.syntax.functor.toFunctorOps  // map
import cats.syntax.flatMap.toFlatMapOps  // flatMap
import cats.syntax.applicative.catsSyntaxApplicativeId   // pure
import cats.syntax.writer.catsSyntaxWriterId           // tell

// 9.3 The Identity Monad
def sumSquare[F[_]: cats.Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a // flatMap
    y <- b // map
  } yield x * x + y * y

sumSquare(Option(3), Option(4))
sumSquare(List(1, 2, 3), List(4, 5))

/*
Id allows us to call our monadic method using plain values.
Scala cannot unify types and type constructors when searching for given instances.
Hence our need to re-type Int as Id[Int] in the call to sumSquare
 */
sumSquare(3: Id[Int], 4: Id[Int])

// 9.5.1 The MonadError Type Class

type ErrorOr[A] = Either[String, A]

val monadError = MonadError[ErrorOr, String]

val success = monadError.pure(42)
val failure = monadError.raiseError("Badness")

monadError.handleErrorWith(failure):
  case "Badness" =>
    monadError.pure("It's ok")

  case _ =>
    monadError.raiseError("It's not ok")

monadError.handleError(failure):
  case "Badness" => 42

  case _ => -1

monadError.ensure(success)("Number too low!")(_ > 1000)

// 9.6 The Eval Monad
val now    = Eval.now(math.random() + 1000)
val always = Eval.always(math.random() + 3000)
val later  = Eval.later(math.random() + 2000)

now.value
// res6: Double = 1000.9807553750455
always.value
// res7: Double = 3000.8009045083577
later.value

/*
+----------+--------+--------------------+
|  Scala   |  Cats  |     Properties     |
+----------+--------+--------------------+
| val      | Now    | eager, memoized    |
| def      | Always | lazy, not memoized |
| lazy val | Later  | lazy, memoized     |
+----------+--------+--------------------+

9.6.3 Eval as a Monad

Eval's map and flatMap methods add computations to a chain. In this case, however,
the chain is stored explicitly as a list of functions. The functions aren’t run
until we call Eval's value method to request a result.
 */
val greeting = Eval
  .always { println("Step 1"); "Hello" }
  .map { str =>
    println("Step 2"); s"$str world"
  }

greeting.value

/*
while the semantics of the originating Eval instances are maintained,
mapping functions are always called lazily on demand (def semantics).
 */
val ans = for
  a <- Eval.now { println("Calculating A"); 40 }
  b <- Eval.always { println("Calculating B"); 2 }
yield {
  println("Adding A and B")
  a + b
}

ans.value // first access
ans.value // second access

/*
Eval has a memoize method that allows us to memoize a chain of computations.
The result of the chain up to the call to memoize is cached, whereas
calculations after the call retain their original semantics.
 */
val saying = Eval
  .always { println("Step 1"); "The cat" }
  .map { str =>
    println("Step 2"); s"$str sat on"
  }
  .memoize
  .map { str =>
    println("Step 3"); s"$str the mat"
  }
// saying: Eval[String] = cats.Eval$$anon$4@76343df7

saying.value // first access
// Step 1
// Step 2
// Step 3
// res19: String = "The cat sat on the mat"
saying.value // second access

// 9.7 The Writer Monad
type Logged[A] = Writer[Vector[String], A]
123.pure[Logged]

// If we have a log and no result we can create a Writer[Unit] using the tell syntax.
Vector("msg1", "msg2", "msg3").tell

// If we have both a result and a log, we can either use Writer.apply or the writer syntax.
val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
val b = 123.writer(Vector("msg1", "msg2", "msg3"))

// We can extract the result and log from a Writer using the value and written methods respectively.

val aResult: Int         = a.value
val aLog: Vector[String] = a.written

// We can extract both values at the same time using the run method.
val (log, result) = b.run

/*
9.7.2 Composing and Transforming Writers
The log in a Writer is preserved when we map or flatMap over it.
flatMap appends the logs from the source Writer and the result of the user's sequencing function.
 */
val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

// 9.8 The Reader Monad
final case class Cat(name: String, favoriteFood: String)

val greetKitty: Reader[Cat, String] =
  Reader[Cat, String](_.name).map(name => s"Hello ${name}")

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for
    greet <- greetKitty
    feed  <- feedKitty
  yield s"$greet. $feed."

greetAndFeed(Cat("Garfield", "lasagne"))

// 9.9 The State Monad
val s = State[Int, String] { state =>
  (state, s"The state is $state")
}

// Get the state and the result
val (state, res) = s.run(10).value

// Get the state, ignore the result
val justTheState = s.runS(10).value

// Get the result, ignore the state
val justTheResult = s.runA(10).value

/*
The general model for using the State monad is to represent each step of a computation
as an instance and compose the steps using the standard monad operators.
 */

// get extracts the state as the result
val getDemo = State.get[Int]
getDemo.run(10).value

// set updates the state and returns unit as the result
val setDemo = State.set[Int](30)
setDemo.run(10).value

// pure ignores the state and returns a supplied result
val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

// inspect extracts the state via a transformation function
val inspectDemo = State.inspect[Int, String](x => s"${x}!")
inspectDemo.run(10).value

// modify updates the state using an update function
val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

// 9.10 Defining Custom Monads

/*
Suppose we want to write a method that calls a function until the function indicates it should stop.
The function will return a monad instance because, as we know, monads represent sequencing and many
monads have some notion of stopping.

We can write this method in terms of flatMap.
 */

// Not stack-safe!
def retry[F[_]: cats.Monad, A](start: A)(f: A => F[A]): F[A] =
  f(start).flatMap { a =>
    retry(a)(f)
  }

// The tailRecM method should recursively call itself until the result of fn returns a Right.
def retryTailRecM[F[_]: cats.Monad as m, A](start: A)(f: A => F[A]): F[A] =
  m.tailRecM(start) { a =>
    f(a).map(Left(_))
  }
