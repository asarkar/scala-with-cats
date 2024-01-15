package ch04

import cats.data.Reader as CatsReader
import cats.syntax.applicative.catsSyntaxApplicativeId

/*
4.8.3 Exercise: Hacking on Readers

The classic use of Readers is to build programs that accept a configuration as a parameter.
Let's ground this with a complete example of a simple login system.
Our configuration will consist of two databases: a list of valid users and a list of their passwords.

Start by creating a type alias DbReader for a Reader that consumes a Db as input.

Now create methods that generate DbReaders to look up the username for an Int user ID,
and look up the password for a String username.

Finally create a checkLogin method to check the password for a given user ID.
 */
object Reader:
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = CatsReader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    CatsReader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    CatsReader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username
        .map(checkPassword(_, password))
        .getOrElse(false.pure[DbReader])
    } yield passwordOk
