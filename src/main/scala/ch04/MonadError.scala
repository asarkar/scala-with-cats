package ch04

import cats.{Monad, MonadError as CatsMonadError}
import cats.syntax.applicativeError.catsSyntaxApplicativeErrorId

/*
4.5.4 Exercise: Abstracting
Implement a method validateAdult with the following signature

def validateAdult[F[_]](age: Int)(using me: MonadError[F, Throwable]): F[Int]

When passed an age greater than or equal to 18 it should return that value as a success.
Otherwise it should return a error represented as an IllegalArgumentException.
 */
object MonadError:
  def validateAdult[F[_]](age: Int)(using me: CatsMonadError[F, Throwable]): F[Int] =
    if age >= 18
    then Monad[F].pure(age)
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]
