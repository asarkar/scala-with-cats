package ch08

import cats.syntax.traverse.toTraverseOps
import cats.syntax.functor.toFunctorOps
import cats.Applicative

// traverse only works on sequences of values that have an Applicative.
// In our original code we were traversing a List[Future[Int]].
// There is an applicative for Future so that was fine.
// In this version we are traversing a List[F[Int]].
// We need to prove to the compiler that F has an Applicative.
class UptimeService[F[_]: Applicative](client: UptimeClient[F]):
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
