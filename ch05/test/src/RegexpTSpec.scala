package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class RegexpTSpec extends AnyFunSpec:
  describe("RegexpT"):
    it("matches"):
      RegexpT("a").repeat.matches("a" * 20000) shouldBe true

