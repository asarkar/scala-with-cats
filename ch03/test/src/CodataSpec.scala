package ch03

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class CodataSpec extends AnyFunSpec:
  describe("Codata"):
    it("list"):
      val sum = list()(0, (a, b) => a + b)
      sum shouldBe 6

      val product = list()(1, (a, b) => a * b)
      product shouldBe 6

