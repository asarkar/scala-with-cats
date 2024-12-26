package ch03

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class BoolSpec extends AnyFunSpec:
  describe("Bool"):
    it("and"):
      and(True, True).`if`("yes")("no") shouldBe "yes"
      and(True, False).`if`("yes")("no") shouldBe "no"
      and(False, True).`if`("yes")("no") shouldBe "no"
      and(False, False).`if`("yes")("no") shouldBe "no"

    it("or"):
      or(True, True).`if`("yes")("no") shouldBe "yes"
      or(True, False).`if`("yes")("no") shouldBe "yes"
      or(False, True).`if`("yes")("no") shouldBe "yes"
      or(False, False).`if`("yes")("no") shouldBe "no"

    it("not"):
      not(True).`if`("yes")("no") shouldBe "no"
      not(False).`if`("yes")("no") shouldBe "yes"
