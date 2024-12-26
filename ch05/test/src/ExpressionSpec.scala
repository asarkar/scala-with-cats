package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class ExpressionSpec extends AnyFunSpec:
  describe("Expression"):
    it("eval"):
      val fortyTwo = ((Expression(15.0) + Expression(5.0)) * Expression(2.0) + Expression(2.0)) / Expression(1.0)
      fortyTwo.eval shouldBe 42.0d
