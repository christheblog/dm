package org.cc.dm.datastructure

import org.scalatest.FunSuite


class CountMapTest extends FunSuite {

  import CountMap._

  test("An empty CountMap should have a size of zero and no keys") {
    val cm = empty[String]
    assert(keys(cm).isEmpty)
  }

  test("An CountMap should increment/decrement correctly the number of keys seen") {
    val cm = from("A","A","A","B","B","B","B","B","C","A")
    assert(keys(cm).size==3)
    assert(count(cm)("A")==4)
    assert(count(cm)("B")==5)
    assert(count(cm)("C")==1)
    // Incrementing
    val inc1 = increment(cm)("A")
    assert(count(inc1)("A")==5)
    // Decrementint
    val dec1 = decrement(cm)("A")
    assert(count(dec1)("A")==3)

  }

  test("Merging 2 count maps together should accurately merges the keys") {
    val cm1 = from("A","A","A","B","B","B","B","B","C","A")
    val cm2 = from("D","D","D","E","E","E","E","E","F","A")
    val cm = merge2(cm1,cm2)
    assert(keys(cm).size==6)
    assert(count(cm)("A")==5) // 4 from cm1 + 1 from cm2
    assert(count(cm)("B")==5)
    assert(count(cm)("C")==1)
    assert(count(cm)("D")==3)
    assert(count(cm)("E")==5)
    assert(count(cm)("F")==1)
  }

}
