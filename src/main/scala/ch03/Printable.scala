package ch03

/*
3.5.5.1 Exercise: Showing off with Contramap
Implement the contramap method for Printable above.
 */
trait Printable[A]:
  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] =
    val self = this
    new Printable[B]:
      def format(value: B): String =
        self.format(f(value))

def format[A](value: A)(using p: Printable[A]): String =
  p.format(value)

object Printable:
  final case class Box[A](value: A)

object PrintableInstances:
  given Printable[String] with
    def format(value: String): String =
      s"'${value}'"

  given Printable[Boolean] with
    def format(value: Boolean): String =
      if (value) "yes" else "no"

  /*
  Define an instance of Printable for the following Box case class.
  Rather than writing out the complete definition from scratch
  (new Printable[Box] etc...), create your instance from an existing
  instance using contramap.
   */
  // If we use `with`, method format has to be defined.
  // Given a Printable[A], and a Box[A], we can create
  // Printable[Box[A]] by the reverse mapping Box[A] => A.
  // The forward mapping would be A => Box[A].
  given [A](using p: Printable[A]): Printable[Printable.Box[A]] =
    p.contramap[Printable.Box[A]](_.value)

import PrintableInstances.given

val ps = format("hello")
val pb = format(true)
val pc = format(Printable.Box("hello"))
