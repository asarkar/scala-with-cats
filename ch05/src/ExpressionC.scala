package ch05

// Continuation-Passing style.
/*  Exercise: CPS Arithmetic */
enum ExpressionC:
  case Literal(value: Double)
  case Addition(left: ExpressionC, right: ExpressionC)
  case Subtraction(left: ExpressionC, right: ExpressionC)
  case Multiplication(left: ExpressionC, right: ExpressionC)
  case Division(left: ExpressionC, right: ExpressionC)

  def +(that: ExpressionC): ExpressionC =
    Addition(this, that)

  def -(that: ExpressionC): ExpressionC =
    Subtraction(this, that)

  def *(that: ExpressionC): ExpressionC =
    Multiplication(this, that)

  def /(that: ExpressionC): ExpressionC =
    Division(this, that)

  def eval: Double =
    type Continuation = Double => Double

    def loop(expr: ExpressionC, cont: Continuation): Double =
      expr match
        case Literal(value) => cont(value)
        case Addition(left, right) =>
          loop(left, l => loop(right, r => cont(l + r)))
        case Subtraction(left, right) =>
          loop(left, l => loop(right, r => cont(l - r)))
        case Multiplication(left, right) =>
          loop(left, l => loop(right, r => cont(l * r)))
        case Division(left, right) =>
          loop(left, l => loop(right, r => cont(l / r)))

    loop(this, identity)

object ExpressionC:
  def apply(value: Double): ExpressionC =
    Literal(value)
