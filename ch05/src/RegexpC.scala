package ch05

// Continuation-Passing style.
enum RegexpC:
  case Append(left: RegexpC, right: RegexpC)
  case OrElse(first: RegexpC, second: RegexpC)
  case Repeat(source: RegexpC)
  case Apply(string: String)
  case Empty

  def ++(that: RegexpC): RegexpC =
    Append(this, that)

  def orElse(that: RegexpC): RegexpC =
    OrElse(this, that)

  def repeat: RegexpC =
    Repeat(this)

  def `*`: RegexpC = this.repeat

  def matches(input: String): Boolean =
    // Define a type alias so we can easily write continuations.
    type Continuation = Option[Int] => Option[Int]

    def loop(
        regexp: RegexpC,
        idx: Int,
        cont: Continuation
    ): Option[Int] =
      regexp match
        case Append(left, right) =>
          val k: Continuation = _ match
            case None    => cont(None)
            case Some(i) => loop(right, i, cont)
          loop(left, idx, k)

        case OrElse(first, second) =>
          val k: Continuation = _ match
            case None => loop(second, idx, cont)
            case some => cont(some)
          loop(first, idx, k)

        case Repeat(source) =>
          val k: Continuation =
            _ match
              case None    => cont(Some(idx))
              case Some(i) => loop(regexp, i, cont)
          loop(source, idx, k)

        case Apply(string) =>
          cont(Option.when(input.startsWith(string, idx))(idx + string.size))

        case Empty =>
          cont(None)

    // Check we matched the entire input
    loop(this, 0, identity).map(_ == input.size).getOrElse(false)

object RegexpC:
  val empty: RegexpC = Empty

  def apply(string: String): RegexpC =
    Apply(string)
