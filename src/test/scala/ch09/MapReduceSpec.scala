package ch09

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MapReduceSpec extends AnyFunSpec:
  it("should run jobs in parallel and reduce the results"):
    val future: Future[Int] =
      MapReduce.parallelFoldMap((1 to 1000).toVector)(_ * 1000)

    val actual = Await.result(future, 1.second)
    actual `shouldBe` 500500000
