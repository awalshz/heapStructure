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
      k <- arbitrary[A]
      //v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("min delete") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("finding Minimums") = forAll {(h: H) =>
    def minList(j: H, acc: List[Int]): List[Int] =
    if (isEmpty(j))  acc else minList(deleteMin(j), findMin(j) :: acc)
   def listIsOrder(list: List[Int]): Boolean =
     if (list.isEmpty || list.tail.isEmpty) true
     else if (list.head < list.tail.head) false
     else listIsOrder(list.tail)
   listIsOrder(minList(h, List()))
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("melding") = forAll { (h: H, j: H) =>
    (isEmpty(h) || isEmpty(j) )|| (findMin(meld(h, j)) == (findMin(h) min findMin(j)))
  }

  // I supprime the minimun, next, I include it, the minimun of the new lis is the same of the old one
  property("same minimun") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val a1 = a min b
    val b1 = a max b
    val a2 = findMin(h)
    val b2 = findMin(deleteMin(h))
    (a1 == a2) && (b1 == b2)
  }

  property("basic") = forAll { (list: List[Int]) =>
    val lord = list.sorted.reverse
    def doheap(l: List[Int], h: H): H = {
      if (l.isEmpty) h
      else doheap(l.tail, insert(l.head, h))
    }
    val h = doheap(list, empty)
    def minList(j: H, acc: List[Int]): List[Int] =
      if (isEmpty(j))  acc else minList(deleteMin(j), findMin(j) :: acc)
    val lord2 = minList(h, List())
    lord == lord2
  }



}
