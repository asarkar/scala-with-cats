package ch05

/*
The core of the interpreter strategy is a separation between description and action.
The description is the program, and the interpreter is the action that carries out the program.

This separation is allows for composition of programs, and managing effects by delaying
them till the time the program is run. We sometimes call this structure an algebra, with
constructs and combinators defining programs and destructors defining interpreters.
The interpreter is then a structural recursion over this ADT.

We saw that the straightforward implementation is not stack-safe, and which caused us to
to introduction the idea of tail recursion and continuations. We reified continuations
functions, and saw that we can convert any program into continuation-passing style which
has every method call in tail position. Due to Scala runtime limitations not all calls
in tail position can be converted to tail calls, so we reified calls and returns into
data structures used by a recursive loop called a trampoline.
 */
enum Expression:
  case Literal(value: Double)
  case Addition(left: Expression, right: Expression)
  case Subtraction(left: Expression, right: Expression)
  case Multiplication(left: Expression, right: Expression)
  case Division(left: Expression, right: Expression)

  def +(that: Expression): Expression =
    Addition(this, that)

  def -(that: Expression): Expression =
    Subtraction(this, that)

  def *(that: Expression): Expression =
    Multiplication(this, that)

  def /(that: Expression): Expression =
    Division(this, that)

  def eval: Double =
    this match
      case Literal(value)              => value
      case Addition(left, right)       => left.eval + right.eval
      case Subtraction(left, right)    => left.eval - right.eval
      case Multiplication(left, right) => left.eval * right.eval
      case Division(left, right)       => left.eval / right.eval

object Expression:
  def apply(value: Double): Expression =
    Literal(value)
