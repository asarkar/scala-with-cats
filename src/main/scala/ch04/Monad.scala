package ch04

trait Monad[F[_]]:
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  /*
  4.1.2 Exercise: Getting Func-y
  Every monad is also a functor. We can define map in the same way
  for every monad using the existing methods, flatMap and pure.
  Try defining map yourself now.
   */
  def map[A, B](value: F[A])(f: A => B): F[B] =
    flatMap(value)(a => pure(f(a)))

  object MonadInstances:
    type Id[A] = A

    /*
    4.3.1 Exercise: Monadic Secret Identities
    Implement pure, map, and flatMap for Id!
     */
    given idMonad: Monad[Id] with
      def pure[A](a: A): Id[A] = a

      def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] =
        f(value)
