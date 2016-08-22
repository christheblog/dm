package org.cc.dm.datastructure

import org.scalatest.FunSuite

class KHeapTest extends FunSuite {

  import KHeap._

  test("Empty heap has size 0") {
    assert(size(empty[Int]) == 0)
  }

  test("Heap with built from n element (n < k) should have a size of n") {
    val heap = from(k=10)(1,2,3,4,5,6,7)
    assert(size(heap) == 7)
  }

  test("Building a heap with restricted size k from a set of n > k elements should have size k") {
    val k = 5
    val heap = from(k)(10,4,6,2,5,3,7,8,9,2,1,5,9)
    assert(size(heap) == k)
  }

  // We want descending order by default - so use is straightforward for KNN-like algorithm
  test("Building a heap of Int will use natural ordering, and element will be sorted in descending order") {
    val heap = from(k=5)(10,4,6,2,5,3,7,8,9,2,1,5,9)
    assert(toStream(heap)==Stream(4,3,2,2,1))
  }

  // We want descending order by default - so use is straightforward for KNN-like algorithm
  test("Restricting to k the size on an existing heap with size > k, should bring the size of the new heap down to k") {
    val heap = from(k=10)(10,4,6,2,5,3,7,8,9,2,1,5,9)
    assert(size(heap)==10)
    assert(toStream(heap)==Stream(8,7,6,5,5,4,3,2,2,1))
    assert(size(restrict(5)(heap))==5)
    assert(toStream(restrict(5)(heap))==Stream(4,3,2,2,1))
  }

}
