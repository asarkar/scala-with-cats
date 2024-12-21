package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class ExpressionCSpec extends AnyFunSpec:
  describe("ExpressionC"):
    it("eval"):
      val fortyTwo = ((ExpressionC(15.0) + ExpressionC(5.0)) * ExpressionC(2.0) + ExpressionC(2.0)) / ExpressionC(1.0)
      fortyTwo.eval shouldBe 42.0d

