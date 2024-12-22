package ch05

/* Exercise: Trampolined Arithmetic */
enum ExpressionT:
  case Literal(value: Double)
  case Addition(left: ExpressionT, right: ExpressionT)
  case Subtraction(left: ExpressionT, right: ExpressionT)
  case Multiplication(left: ExpressionT, right: ExpressionT)
  case Division(left: ExpressionT, right: ExpressionT)

  def eval: Double =
    // Trampoline style.
    type Continuation = Double => Call

    enum Call:
      case Continue(value: Double, k: Continuation)
      case Loop(expr: ExpressionT, k: Continuation)
      case Done(result: Double)

    def loop2(left: ExpressionT, right: ExpressionT, cont: Continuation, op: (Double, Double) => Double): Call =
      Call.Loop(
        left,
        l => Call.Loop(right, r => Call.Continue(op(l, r), cont))
      )

    def loop(expr: ExpressionT, cont: Continuation): Call =
      expr match
        case Literal(value)              => Call.Continue(value, cont)
        case Addition(left, right)       => loop2(left, right, cont, _ + _)
        case Subtraction(left, right)    => loop2(left, right, cont, _ - _)
        case Multiplication(left, right) => loop2(left, right, cont, _ * _)
        case Division(left, right)       => loop2(left, right, cont, _ / _)

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
