package org.cc.dm.algo.majority

import collection.immutable.Seq
import org.scalatest.FunSuite

import org.cc.dm.data._
import org.cc.dm.algo.majority.Majority._


class MajorityTest extends FunSuite {

  // Testing classic implementation

  test("Majority classifier returns None if the dataset is empty") {
    assert(classify(model(empty))(vec(Seq(1.0,2.0))).isEmpty)
  }

  test("Majority classifier returns Some(something) if the dataset is NOT empty") {
    val data = ds[String,SVec[String]](Seq(vec("A",Seq(1.0,2.0,3.0))))
    val target = vec(Seq[Double](1.0,2.0))
    assert(classify(model(data))(target).isDefined)
    assert(Some("A") === classify(model(data))(target))
  }

  test("Majority classifier returns the majority class") {
    val data = ds[String,SVec[String]](Seq(
      vec("A",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("A",Seq(1.0,2.0,3.0)),
      vec("C",Seq(1.0,2.0,3.0))
    ))
    val target = vec(Seq[Double](1.0,2.0))
    assert(Some("B") === classify(model(data))(target))
  }

  test("Majority classifier returns one of the majority class if there is a tie") {
    val data = ds[String,SVec[String]](Seq(
      vec("A",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("B",Seq(1.0,2.0,3.0)),
      vec("A",Seq(1.0,2.0,3.0)),
      vec("A",Seq(1.0,2.0,3.0)),
      vec("C",Seq(1.0,2.0,3.0))
    ))
    val target = vec(Seq[Double](1.0,2.0))
    assert(
      classify(model(data))(target).exists(_=="A") ||
      classify(model(data))(target).exists(_=="B")
    )
  }

  def empty[C,T <: SVec[C]](): SDataset[C,T] = Seq[T]()
  def ds[C,T <: SVec[C]](xs: Seq[T]): SDataset[C,T] = xs

}
