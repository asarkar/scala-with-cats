package ch01.show

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.show.toShow

class CatSpec extends AnyFunSpec:
  it("Cat should use Show to print the cat"):
    Cat("Garfield", 41, "ginger and black").show `shouldBe`
      "Garfield is a 41 year-old ginger and black cat."
