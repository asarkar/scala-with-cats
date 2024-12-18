package ch02

/*
Structural corecursion works by considering all the possible outputs, which are the constructors of
the algebraic data type, and then working out the conditions under which we’d call each constructor.

We could abstract structural corecursion as an unfold.
 */
enum MyList[A]:
  case Empty()
  case Pair(_head: A, _tail: MyList[A])

  def map[B](f: A => B): MyList[B] =
//    this match
//      case Empty()          => Empty()
//      case Pair(head, tail) => Pair(f(head), tail.map(f))
    MyList.unfold(this)(_.isEmpty, a => f(a.head), _.tail)

  def isEmpty: Boolean =
    this match
      case Empty() => true
      case _       => false

  def head: A =
    this match
      case Pair(head, _) => head
      case _             => scala.sys.error("empty list")

  def tail: MyList[A] =
    this match
      case Pair(_, tail) => tail
      case _             => scala.sys.error("empty list")

  def fold[B](zero: B)(f: (A, B) => B): B =
    this match
      case Empty()          => zero
      case Pair(head, tail) => f(head, tail.fold(zero)(f))

  def toSeq: Seq[A] = fold(Seq.empty)(_ +: _)

object MyList:
  /*
  Types inferred for one method parameter cannot be used for other method parameters in the same parameter list.
  However, types inferred for one method parameter list can be used in subsequent lists.
   */
  def unfold[A, B](seed: A)(stop: A => Boolean, f: A => B, next: A => A): MyList[B] =
    if stop(seed) then MyList.Empty()
    else MyList.Pair(f(seed), unfold(next(seed))(stop, f, next))

  def fill[A](n: Int)(elem: => A): MyList[A] =
    unfold(0)(_ == n, _ => elem, _ + 1)

  def iterate[A](start: A, len: Int)(f: A => A): MyList[A] =
    unfold((0, start))(_._1 == len, _._2, (i, a) => (i + 1, f(a)))
