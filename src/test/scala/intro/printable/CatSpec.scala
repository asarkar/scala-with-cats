package intro.printable

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import PrintableSyntax.*

class CatSpec extends AnyFunSpec:
  describe("Cat"):
    it("should use Printable to print the cat"):
      Cat("Garfield", 41, "ginger and black").format `shouldBe`
        "Garfield is a 41 year-old ginger and black cat."
