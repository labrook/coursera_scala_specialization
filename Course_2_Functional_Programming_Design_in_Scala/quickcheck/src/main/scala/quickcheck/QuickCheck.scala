package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min of 2 inserted elements = min of heap") = forAll { (h: H) =>
    val x1 = arbitrary[A].sample.get
    val x2 = arbitrary[A].sample.get
    val m = if (ord.lteq(x1, x2)) x1 else x2
    findMin(insert(x1, insert(x2, empty))) == m
  }

  // If you insert an element into an empty heap,
  // then delete the minimum, the resulting heap should be empty.
  property("insert then delete = empty heap") = forAll { (h: H) =>
    val x = arbitrary[A].sample.get
    deleteMin(insert(x, empty)) == empty
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  property("sequential minima = sorted sequence") = forAll { (h: H) =>
    def seqMinHelper(he: H, acc: List[A]): List[A] = {
      if (isEmpty(he)) acc
      else seqMinHelper(deleteMin(he), findMin(he) :: acc)
    }
    val seqMin = seqMinHelper(h, Nil)
    seqMin == seqMin.sortWith(_ > _ )
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("minimum of the melding is minimum of one or the other") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) | isEmpty(h2)) true
    else {
      val m = findMin(meld(h1, h2))
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      m == m1 || m == m2
    }
  }

  property("order of adding elements should not matter") = forAll{ (h: H) =>
    val x1 = arbitrary[A].sample.get
    val x2 = arbitrary[A].sample.get
    val x3 = arbitrary[A].sample.get
    val h123 = insert(x3, insert(x2, insert(x1, empty)))
    val h321 = insert(x1, insert(x2, insert(x3, empty)))

    def seqMinHelper(he: H, acc: List[A]): List[A] = {
      if (isEmpty(he)) acc
      else seqMinHelper(deleteMin(he), findMin(he) :: acc)
    }
    val seqMin123 = seqMinHelper(h123, Nil)
    val seqMin321 = seqMinHelper(h321, Nil)

    seqMin123 == seqMin321
  }





}
