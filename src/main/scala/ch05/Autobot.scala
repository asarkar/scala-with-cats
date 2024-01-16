package ch05

import scala.concurrent.Future
import cats.data.EitherT
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.*

/*
5.4 Exercise: Monads: Transform and Roll Out

The Autobots, well-known robots in disguise, frequently send messages
during battle requesting the power levels of their team mates.
This helps them coordinate strategies and launch devastating attacks.

Transmissions take time in Earth’s viscous atmosphere, and messages are
occasionally lost due to satellite malfunction or sabotage by pesky Decepticons8.

Optimus Prime is getting tired of the nested for comprehensions in his neural matrix.
Help him by rewriting Response using a monad transformer.
 */
object Autobot:

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  /*
  Implement getPowerLevel to retrieve data from a set of imaginary allies.
  If an Autobot isn’t in the powerLevels map, return an error message reporting
  that they were unreachable. Include the name in the message for good effect.
   */
  def getPowerLevel(ally: String): Response[Int] =
    powerLevels.get(ally) match
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))

  /*
  Two autobots can perform a special move if their combined power level is greater than 15.
  If either ally is unavailable, fail with an appropriate error message.
   */
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for
      lvl1 <- getPowerLevel(ally1)
      lvl2 <- getPowerLevel(ally2)
    yield (lvl1 + lvl2) > 15

  /*
  Write a method tacticalReport that takes two ally names and prints
  a message saying whether they can perform a special move.
   */
  def tacticalReport(ally1: String, ally2: String): String =
    val stack: Future[Either[String, Boolean]] =
      canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match
      case Left(msg)    => s"Comms error: $msg"
      case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
