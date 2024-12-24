package ch09

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe}
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.EitherValues.convertLeftProjectionToValuable
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class LibSpec extends AnyFunSpec:
  describe("MonadError"):
    it("validateAdult"):    
        Lib.validateAdult[Try](18).success.value shouldBe 18
        Lib.validateAdult[Try](8).failure.exception shouldBe an[IllegalArgumentException]
        type ExceptionOr[A] = Either[Throwable, A]
        Lib.validateAdult[ExceptionOr](-1).left.value shouldBe an[IllegalArgumentException]
        
  describe("Writer"):
    it("factorial should maintain the order of logging"):
      val computations = Future.sequence(
        Vector(
          Future(Lib.factorial(5)),
          Future(Lib.factorial(5))
        )
      )
      val actual = Await.result(
        computations.map(_.map(_.written)),
        5.seconds
      )
      val logs = Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120")
      actual shouldBe Vector(logs, logs)

  describe("Reader"):
    it("DbReader should check password"):
      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )

      val passwords = Map(
        "dade"  -> "zerocool",
        "kate"  -> "acidburn",
        "margo" -> "secret"
      )

      val db = Lib.Db(users, passwords)

      Lib.checkLogin(1, "zerocool").run(db) shouldBe true
      Lib.checkLogin(4, "davinci").run(db) shouldBe false

  describe("State"):
    it("evalInput should evaluate a post-order expression"):
      Lib.evalInput("1 2 + 3 4 + *") shouldBe 21
