package ch04

import cats.data.Writer as CatsWriter
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.writer.catsSyntaxWriterId

/*
4.7.3 Exercise: Show Your Working

Rewrite factorial so it captures the log messages in a Writer.
Demonstrate that this allows us to reliably separate the logs for concurrent computations.
 */
object Writer:
  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  type Logged[A] = CatsWriter[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for
      ans <-
        if (n == 0)
        then 1.pure[Logged]
        else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    yield ans
