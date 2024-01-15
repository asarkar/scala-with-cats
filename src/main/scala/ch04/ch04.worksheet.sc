import cats.Eval
import cats.data.{Reader, Writer, State}
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.writer.catsSyntaxWriterId

// ----------------------------------------------------------------------------
//                                     Eval
// ----------------------------------------------------------------------------

// call-by-value which is eager and memoized
val now = Eval.now(math.random + 1000)
// call-by-name which is lazy and not memoized
val always = Eval.always(math.random + 3000)
// call-by-need which is lazy and memoized
val later = Eval.later(math.random + 2000)

now.value
always.value
later.value

val greeting = Eval
  .always{ println("Step 1"); "Hello" }
  .map{ str => println("Step 2"); s"$str world" }

greeting.value
// Step 1
// Step 2
// res16: String = "Hello world"

val ans = for {
  a <- Eval.now{ println("Calculating A"); 40 }
  b <- Eval.always{ println("Calculating B"); 2 }
} yield {
  println("Adding A and B")
  a + b
}

ans.value // first access
// Calculating B
// Adding A and B
// res17: Int = 42 // first access
ans.value // second access
// Calculating B
// Adding A and B
// res18: Int = 42

val saying = Eval
  .always{ println("Step 1"); "The cat" }
  .map{ str => println("Step 2"); s"$str sat on" }
  .memoize
  .map{ str => println("Step 3"); s"$str the mat" }

saying.value // first access
// Step 1
// Step 2
// Step 3
// res19: String = "The cat sat on the mat" // first access
saying.value // second access
// Step 3
// res20: String = "The cat sat on the mat"

// stack-safe
def factorial(n: BigInt): Eval[BigInt] =
  if(n == 1) {
    Eval.now(n)
  } else {
    Eval.defer(factorial(n - 1).map(_ * n))
  }

factorial(50000).value

// stack-safe foldRight
ch04.Eval.foldRight((1 to 100000).toList, 0L)(_ + _)

// ----------------------------------------------------------------------------
//                                    Writer
// ----------------------------------------------------------------------------
type Logged[A] = Writer[Vector[String], A]

123.pure[Logged]

Vector("msg1", "msg2", "msg3").tell

val b = 123.writer(Vector("msg1", "msg2", "msg3"))

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

writer2.run

val writer3 = writer1.bimap(
  log => log.map(_.toUpperCase),
  res => res * 100
)

writer3.run

val writer5 = writer1.reset

writer5.run

// ----------------------------------------------------------------------------
//                                     Reader
// ----------------------------------------------------------------------------
final case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello ${name}")

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed  <- feedKitty
  } yield s"$greet. $feed."

greetAndFeed(Cat("Garfield", "lasagne"))

// ----------------------------------------------------------------------------
//                                     State
// ----------------------------------------------------------------------------

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

// Get the state and the result
val (state, result) = a.run(10).value

// Get the state, ignore the result
val justTheState = a.runS(10).value

// Get the result, ignore the state
val justTheResult = a.runA(10).value
