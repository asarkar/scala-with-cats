package ch08

/*
8.6.1.1 Exercise: Showing off with Contramap
Implement the contramap method for Display above.
 */
trait Display[A]:
  def display(value: A): String

  def contramap[B](f: B => A): Display[B] =
    val self = this
    new Display[B]:
      def display(value: B): String =
        self.display(f(value))

def display[A: Display as d](value: A): String =
  d.display(value)

object Display:
  final case class Box[A](value: A)

  given Display[String]:
    def display(value: String): String =
      s"'${value}'"

  given Display[Boolean]:
    def display(value: Boolean): String =
      if (value) "yes" else "no"

  /*
  Define an instance of Display for the following Box case class.
  Rather than writing out the complete definition from scratch
  (new Display[Box] etc...), create your instance from an existing
  instance using contramap.
   */
  // Given a Display[A], and a Box[A], we can create
  // Display[Box[A]] by the reverse mapping Box[A] => A.
  // The forward mapping would be A => Box[A].

  // Parameterized typeclass with named context bound.
  given [A: Display as d] => Display[Box[A]] =
    d.contramap[Box[A]](_.value)
