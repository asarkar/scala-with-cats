package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class ExpressionTSpec extends AnyFunSpec:
  describe("ExpressionT"):
    it("eval"):
      val fortyTwo = ((ExpressionT(15.0) + ExpressionT(5.0)) * ExpressionT(2.0) + ExpressionT(2.0)) / ExpressionT(1.0)
      fortyTwo.eval shouldBe 42.0d

