package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.prop.TableDrivenPropertyChecks.*

class RegexpCSpec extends AnyFunSpec:
  describe("RegexpC"):
    it("matches"):
      val txts =
        Table(
            ("txt", "match"),
            ("Scala", true),
            ("Scalalalala", true),
            ("Sca", false),
            ("Scalal", false),
            ("Scalaland", false)
        )
      // left-associative
      val regexp = RegexpC("Sca") ++ RegexpC("la") ++ RegexpC("la").repeat

      forAll (txts) { (txt: String, `match`: Boolean) =>
        regexp.matches(txt) shouldBe `match`
      }

