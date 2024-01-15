package ch01.eq

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.eq.catsSyntaxEq

class CatSpec extends AnyFunSpec:
  it("Cat should use Eq for equality"):
    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 32, "orange and black")

    cat1 === cat2 `shouldBe` false
    cat1 =!= cat2 `shouldBe` true

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    optionCat1 === optionCat2 `shouldBe` false
    optionCat1 =!= optionCat2 `shouldBe` true
