package ch01.printable

/*
1.3 Exercise: Printable Library.

Let's define a Printable type class:

1. Define a type class Printable[A] containing a single method format.
   format should accept a value of type A and return a String.

2. Create an object PrintableInstances containing instances of Printable for String and Int.

3. Define an object Printable with two generic interface methods:

   format accepts a value of type A and a Printable of the corresponding type.
   It uses the relevant Printable to convert the A to a String.

   print accepts the same parameters as format and returns Unit.
   It prints the formatted A value to the console using println.
 */
trait Printable[A]:
  def format(a: A): String

object PrintableInstances:
  given Printable[String] with
    override def format(s: String): String = s

  given Printable[Int] with
    override def format(i: Int): String = i.toString()

/*
Let's make our printing library easier to use by defining some extension
methods to provide better syntax:

1. Create an object called PrintableSyntax.

2. Inside PrintableSyntax define an implicit class PrintableOps[A] to wrap up a value of type A.

3. In PrintableOps define the following methods:

   format accepts an implicit Printable[A] and returns a String representation of the wrapped A;

   print accepts an implicit Printable[A] and returns Unit. It prints the wrapped A to the console.
 */
object PrintableSyntax:
  extension [A](a: A)(using p: Printable[A])
    def format: String =
      p.format(a)

    def print(): Unit =
      println(a.format)
