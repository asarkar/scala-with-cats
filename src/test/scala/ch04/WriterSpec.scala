package ch04
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class WriterSpec extends AnyFunSpec:
  it("factorial should maintain the order of logging"):
    val computations = Future.sequence(
      Vector(
        Future(Writer.factorial(5)),
        Future(Writer.factorial(5))
      )
    )
    val actual = Await.result(
      computations.map(_.map(_.written)),
      5.seconds
    )
    val logs = Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120")
    actual `shouldBe` Vector(logs, logs)
