package ch05

enum ExpressionT:
  case Literal(value: Double)
  case Addition(left: ExpressionT, right: ExpressionT)
  case Subtraction(left: ExpressionT, right: ExpressionT)
  case Multiplication(left: ExpressionT, right: ExpressionT)
  case Division(left: ExpressionT, right: ExpressionT)

  import ExpressionT.*
  def eval: Double =
    def loop(expr: ExpressionT, cont: Continuation): Call =
      expr match
        case Literal(value) => Call.Continue(value, cont)
        case Addition(left, right) =>
          Call.Loop(
            left,
            l => Call.Loop(right, r => Call.Continue(l + r, cont))
          )
        case Subtraction(left, right) =>
          Call.Loop(
            left,
            l => Call.Loop(right, r => Call.Continue(l - r, cont))
          )
        case Multiplication(left, right) =>
          Call.Loop(
            left,
            l => Call.Loop(right, r => Call.Continue(l * r, cont))
          )
        case Division(left, right) =>
          Call.Loop(
            left,
            l => Call.Loop(right, r => Call.Continue(l / r, cont))
          )

    def trampoline(call: Call): Double =
      call match
        case Call.Continue(value, k) => trampoline(k(value))
        case Call.Loop(expr, k)      => trampoline(loop(expr, k))
        case Call.Done(result)       => result

    trampoline(loop(this, x => Call.Done(x)))

  def +(that: ExpressionT): ExpressionT =
    Addition(this, that)

  def -(that: ExpressionT): ExpressionT =
    Subtraction(this, that)

  def *(that: ExpressionT): ExpressionT =
    Multiplication(this, that)

  def /(that: ExpressionT): ExpressionT =
    Division(this, that)

object ExpressionT:
  def apply(value: Double): ExpressionT =
    Literal(value)

  // Trampoline style.
  type Continuation = Double => Call

  enum Call:
    case Continue(value: Double, k: Continuation)
    case Loop(expr: ExpressionT, k: Continuation)
    case Done(result: Double)
