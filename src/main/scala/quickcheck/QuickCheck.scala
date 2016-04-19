package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minTwo") = forAll { (a: Int, b: Int) =>
    (a > b) ==> {
      val h = insert(b, insert(a, empty))
      ("First min should be #1" |: findMin(h) == b) &&
        ("Second min should be #2" |: findMin(deleteMin(h)) == a)
    }

  }

  property("minThree") = forAll { (a: Int, b: Int, c:Int) =>
    (a > b) ==> {
      true
    }
  }

  property("minMeld") = forAll { (h1: H, h2: H) =>
    val melding = meld(h1, h2)
    val firstMin = findMin(h1)
    val secondMin = findMin(h2)
    val totalMin = if (firstMin < secondMin) firstMin else secondMin
    findMin(melding) == totalMin
  }

  property("addOneThenRemove") = forAll { a: Int =>
    val h2 = deleteMin(insert(a, empty))
    isEmpty(h2)
  }

  property("isSorted") = forAll { h:H =>
    def checkSorted(h:H): Boolean = {
      @tailrec
      def check(h:H, prev: A): Boolean = {
        if (isEmpty(h)) true
        else {
          val current = findMin(h)
          prev <= current && check(deleteMin(h), current)
        }
      }
      check(deleteMin(h), findMin(h))
    }
    checkSorted(h)
  }


  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
