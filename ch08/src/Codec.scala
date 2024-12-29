package ch08

/*
8.6.2.1 Transformative Thinking with imap
Implement the imap method for Codec.
 */
trait Codec[A]:
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    val self = this
    new Codec[B]:
      override def encode(value: B): String =
        self.encode(enc(value))

      override def decode(value: String): B =
        dec(self.decode(value))

def encode[A: Codec as c](value: A): String =
  c.encode(value)

def decode[A: Codec as c](value: String): A =
  c.decode(value)

object Codec:
  final case class Box[A](value: A)

  given stringCodec: Codec[String]:
    def encode(value: String): String = value
    def decode(value: String): String = value

  given Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  // Demonstrate your imap method works by creating a Codec for Double.
  given Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  // Implement a Codec for the Box type.
  given boxCodec: [A: Codec as c] => Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)
