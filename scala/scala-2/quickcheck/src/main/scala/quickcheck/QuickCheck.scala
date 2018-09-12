package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genNonEmptyHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    genNonEmptyHeap
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("nonEmpty") = forAll(genNonEmptyHeap) { (h: H) =>
    !isEmpty(h)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val m = Math.min(a,b)
    val h = insert(b, insert(a, empty))
    findMin(h) == m
  }

  property("empty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("min3") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h1: H, h2: H) =>
    val min = Math.min( findMin(h1), findMin(h2) )
    val h = meld(h1, h2)

    findMin(h) == min
  }

  property("sort") = forAll(genNonEmptyHeap) { (h: H) =>
    val min = findMin(h)
    ordered(deleteMin(h), min)
  }

  property("delete") = forAll { (ints: List[Int] ) =>
    val h = insertAll(ints, empty)
    val xs = deleteAll(h)
    ints.sorted == xs
  }

  private def ordered(h: H, prev: Int): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      if (min < prev) false
      else ordered(deleteMin(h), min)
    }
  }

  def insertAll(xs: List[Int], h: H): H = xs match {
    case List() => h
    case x :: xs1 => insertAll(xs1, insert(x, h))
  }

  def deleteAll(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: deleteAll(deleteMin(h))
  }
}
