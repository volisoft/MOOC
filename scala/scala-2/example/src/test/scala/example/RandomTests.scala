package example

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

@RunWith(classOf[JUnitRunner])
class RandomTests extends PropSpec with Matchers with Checkers with GeneratorDrivenPropertyChecks {
  private val arrayGen = (size: Int) => Gen.containerOfN[Array, Int](size, arbitrary[Int])

//  property("shift array rotation") {
//    forAll((arrayGen(10), "array"), (Gen.posNum[Int], "rotation")) {
//      case (arr, dist) => rotate(arr, dist) should contain theSameElementsInOrderAs rotate(rotate(arr, dist), dist)
//    }
//  }

  property("shift array rotation 2") {
    forAll((arrayGen(10), "array"), (Gen.posNum[Int], "rotation")) {
      case (arr, dist) =>
        val rotated = rotate(arr, dist)
        rotated.drop(dist) should contain theSameElementsInOrderAs arr.dropRight(dist)
        rotated.take(dist) should contain theSameElementsInOrderAs arr.takeRight(dist)
    }
  }


  def rotate(arr: Array[Int], distance: Int) = {
    if (arr.length == 0 || arr.length == 1) arr
    else {
      val arr2 = new Array[Int](arr.length)

      val dist  = distance % arr.length

      System.arraycopy(arr, 0, arr2, dist, arr.length - dist)
      System.arraycopy(arr, arr.length - dist, arr2, 0, dist)

      arr2
    }
  }

}
