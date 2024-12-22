package ch03

trait Stream[A]:
  def head: A
  def tail: Stream[A]
  override def toString: String = head.toString

  def take(count: Int): scala.collection.immutable.List[A] =
    count match
      case 0 => Nil
      case n => head :: tail.take(n - 1)

  def map[B](f: A => B): Stream[B] =
    val self = this
    new Stream[B]:
      def head: B         = f(self.head)
      def tail: Stream[B] = self.tail.map(f)

  /*
  Exercise: Stream Combinators
  Implement filter, zip, and scanLeft on Stream.
   */
  def filter(p: A => Boolean): Stream[A] =
    lazy val self = if p(head) then this else tail.filter(p)
    new Stream[A]:
      def head: A         = self.head
      def tail: Stream[A] = self.tail.filter(p)

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    val self = this
    new Stream[(A, B)]:
      def head: (A, B)         = (self.head, that.head)
      def tail: Stream[(A, B)] = self.tail.zip(that.tail)

  def scanLeft[B](zero: B)(f: (B, A) => B): Stream[B] =
    val self = this
    new Stream[B]:
      def head: B         = zero
      def tail: Stream[B] = self.tail.scanLeft(f(zero, self.head))(f)

object Stream:
  def unfold[A, B](seed: A)(f: A => B, next: A => A): Stream[B] =
    new Stream[B]:
      def head: B         = f(seed)
      def tail: Stream[B] = unfold(next(seed))(f, next)

  val ones: Stream[Int] =
    new Stream[Int]:
      def head: Int = 1

      def tail: Stream[Int] = ones

  val alternating: Stream[Int] = Stream.unfold(true)(if _ then 1 else -1, !_)

  val naturals: Stream[Int] = ones.scanLeft(1)(_ + _)

  val naturals2: Stream[Int] = unfold(1)(x => x, _ + 1)

  val naturals3: Stream[Int] =
    new Stream:
      def head              = 1
      def tail: Stream[Int] = naturals.map(_ + 1)
