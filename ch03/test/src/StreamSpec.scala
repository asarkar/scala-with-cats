package ch03

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class StreamSpec extends AnyFunSpec:
  describe("Stream"):
    it("ones"):
      Stream.ones.take(5) shouldBe List.fill(5)(1)

    it("alternating"):
      Stream.alternating.take(5) shouldBe List.iterate(1, 5)(_ * -1)

    it("filter"):
      Stream.alternating.filter(_ > 0).take(5) shouldBe List.fill(5)(1)

    it("naturals"):
      Stream.naturals.take(5) shouldBe (1 to 5)
      Stream.naturals2.take(5) shouldBe (1 to 5)
      Stream.naturals3.take(5) shouldBe (1 to 5)



