package ch04
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.util.Try
import scala.util.Success

class MonadErrorSpec extends AnyFunSpec:
  it("should check if adult"):
    val actual = MonadError.validateAdult[Try](18)
    actual `shouldBe` Success(18)

    val actual2 = MonadError.validateAdult[Try](8)
    // No easy way to get the exception out
    // except for pattern matching.
    actual2.isFailure `shouldBe` true
