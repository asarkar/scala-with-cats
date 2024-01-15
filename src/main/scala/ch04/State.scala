package ch04

import cats.data.State as CatsState
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.apply.catsSyntaxApplyOps

/*
4.9.3 Exercise: Post-Order Calculator
Let's write an interpreter for post-order expressions.
We can parse each symbol into a State instance representing
a transformation on the stack and an intermediate result.

Start by writing a function evalOne that parses a single symbol into an instance of State.
If the stack is in the wrong configuration, it's OK to throw an exception.
 */
object State:
  type Stack        = List[Int]
  type CalcState[A] = CatsState[Stack, A]

  def eval(sym: String, s: Stack): Stack =
    s match
      case x :: y :: s1 =>
        sym match
          case "+"           => x + y :: s1
          case "-"           => y - x :: s1
          case "*"           => x * y :: s1
          case "/" if x != 0 => y / x :: s1
          case "/"           => sys.error("divide by zero")
      case _ => sys.error("bad expression")

  def evalOne(sym: String): CalcState[Int] =
    for
      s <- CatsState.get[Stack]
      s1 = sym match
        case x if x.forall(Character.isDigit) => x.toInt :: s
        case x                                => eval(sym, s)

      _ <- CatsState.set[Stack](s1)
    yield s1.head

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
