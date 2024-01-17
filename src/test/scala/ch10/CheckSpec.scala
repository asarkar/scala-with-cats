package ch10

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import org.scalatest.EitherValues
import cats.data.NonEmptyList

final case class User(username: String, email: String)

def createUser(username: String, email: String): Either[Errors, User] = (
  checkUsername.run(username),
  checkEmail.run(email)
)
  .mapN(User.apply)

class CheckSpec extends AnyFunSpec with EitherValues:
  it("should pass validation for a valid user form"):
    val usr1 = createUser("John", "john@somewhere.com")
    usr1.value `shouldBe` User("John", "john@somewhere.com")

  it("should fail validation for an invalid user form"):
    val usr1 = createUser("", "john@somewhere.com")
    usr1.left.value `shouldBe` NonEmptyList("Must be longer than 3 characters", Nil)
