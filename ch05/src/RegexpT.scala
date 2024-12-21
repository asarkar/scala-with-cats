package ch05

enum RegexpT:
  def ++(that: RegexpT): RegexpT =
    Append(this, that)

  def orElse(that: RegexpT): RegexpT =
    OrElse(this, that)

  def repeat: RegexpT =
    Repeat(this)

  def `*`: RegexpT = this.repeat

  def matches(input: String): Boolean =
    /*
    Scala's runtimes don't support full tail calls, so calls from a continuation
    to loop or from loop to a continuation will use a stack frame.
    So, instead of making a call, we return a value that reifies the call we want to make.
    This idea is the core of trampolining.
     */
    // Define a type alias so we can easily write continuations.
    type Continuation = Option[Int] => Call

    enum Call:
      case Loop(regexp: RegexpT, index: Int, continuation: Continuation)
      case Continue(index: Option[Int], continuation: Continuation)
      case Done(index: Option[Int])

    def loop(regexp: RegexpT, idx: Int, cont: Continuation): Call =
      regexp match
        case Append(left, right) =>
          val k: Continuation = _ match
            case None    => Call.Continue(None, cont)
            case Some(i) => Call.Loop(right, i, cont)
          Call.Loop(left, idx, k)

        case OrElse(first, second) =>
          val k: Continuation = _ match
            case None => Call.Loop(second, idx, cont)
            case some => Call.Continue(some, cont)
          Call.Loop(first, idx, k)

        case Repeat(source) =>
          val k: Continuation =
            _ match
              case None    => Call.Continue(Some(idx), cont)
              case Some(i) => Call.Loop(regexp, i, cont)
          Call.Loop(source, idx, k)

        // The following could directly call 'cont' with the Option
        // if Scala had support for full tail calls.
        case Apply(string) =>
          Call.Continue(
            Option.when(input.startsWith(string, idx))(idx + string.size),
            cont
          )

        case Empty =>
          Call.Continue(None, cont)

    def trampoline(next: Call): Option[Int] =
      next match
        case Call.Loop(regexp, index, continuation) =>
          trampoline(loop(regexp, index, continuation))
        case Call.Continue(index, continuation) =>
          trampoline(continuation(index))
        case Call.Done(index) => index

    // Check we matched the entire input
    trampoline(loop(this, 0, Call.Done(_)))
      .map(_ == input.size)
      .getOrElse(false)

  case Append(left: RegexpT, right: RegexpT)
  case OrElse(first: RegexpT, second: RegexpT)
  case Repeat(source: RegexpT)
  case Apply(string: String)
  case Empty

object RegexpT:
  val empty: RegexpT = Empty

  def apply(string: String): RegexpT =
    Apply(string)
