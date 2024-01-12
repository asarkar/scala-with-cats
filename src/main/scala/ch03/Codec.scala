package ch03

/*
3.5.6.1 Transformative Thinking with imap
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

def encode[A](value: A)(using c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(using c: Codec[A]): A =
  c.decode(value)

object Codec:
  final case class Box[A](value: A)

object CodecInstances:
  given stringCodec: Codec[String] with
    def encode(value: String): String = value
    def decode(value: String): String = value

  given Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  // Demonstrate your imap method works by creating a Codec for Double.
  given Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  // Implement a Codec for the Box type.
  given boxCodec[A](using c: Codec[A]): Codec[Codec.Box[A]] =
    c.imap[Codec.Box[A]](Codec.Box(_), _.value)

import CodecInstances.given

val ca = encode(123.4)
val cb = decode[Double]("123.4")

val cc = encode(Codec.Box(123.4))
val cd = decode[Codec.Box[Double]]("123.4")
