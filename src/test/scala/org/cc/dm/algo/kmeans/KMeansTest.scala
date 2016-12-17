package org.cc.dm.algo.kmeans

import collection.immutable.{IndexedSeq,Seq}
import org.cc.dm.data._
import org.cc.dm.algo.kmeans.KMeans._
import org.cc.dm.data
import org.scalatest.FunSuite

class KMeansTest extends FunSuite {

  val Euclidean = (v1: RealVec, v2: RealVec) => math.sqrt((v1 zip v2).map { case (x,y) => (x-y) * (x-y)}.sum)

  // Testing classic implementation

  // Documenting behavior ....
  test("First iteration of KMeans on an empty set should return the initial clusters") {
    val ds = dataset()
    val clusters = Seq(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )

    val kmIter = kmeans(ds)(distfn=Euclidean)(clusters=clusters)
    assert(clusters === kmIter.head)
  }

  // Documenting behavior on an empty set ....
  test("From second iteration of KMeans on an empty, the cluster set should be empty") {
    val ds = dataset()
    val clusters = Seq(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )

    val kmIter = kmeans(ds)(distfn=Euclidean)(clusters=clusters)
    assert(kmIter.tail.head.isEmpty)
  }


  // Checking convergence
  test("Checking KMeans iteration on a dataset equivalent to the initial clusters. Should converge to the clusters") {
    val clusters = Seq(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )
    val ds = dataset(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )

    val kmIter = kmeans(ds)(distfn=Euclidean)(clusters=clusters)
    assert(clusters === kmIter.head)
    assert(clusters === kmIter.tail.head)
    assert(clusters === kmIter.take(100).head)
  }

  test("Checking KMeans iteration on a known dataset with 1 element per cluster") {
    val clusters = Seq(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )
    val data = Seq(
      vec(0.1,0.1,0.1,0.1),
      vec(1.1,1.1,1.1,1.1),
      vec(2.1,2.1,2.1,2.1)
    )
    val ds = dataset(data:_*)

    val kmIter = kmeans(ds)(distfn=Euclidean)(clusters=clusters)
    assert(data === kmIter.tail.head)
  }

  test("KMeans should compile and work on a dataset of supervised vectors") {
    val clusters = Seq(
      vec(0.0,0.0,0.0,0.0),
      vec(1.0,1.0,1.0,1.0),
      vec(2.0,2.0,2.0,2.0)
    )
    val data = Seq(
      svec("A",0.1,0.1,0.1,0.1),
      svec("A",1.1,1.1,1.1,1.1),
      svec("B",2.1,2.1,2.1,2.1)
    )
    val ds = dataset(data:_*)

    val kmIter = kmeans(ds)(distfn=Euclidean)(clusters=clusters)
    assert(data === kmIter.take(10).head)
  }

  // Helper

  def dataset(xs: RealVec*): RealDataset = IndexedSeq(xs:_*)
  def vec(xs: Double*): RealVec = IndexedSeq(xs:_*)
  def svec(cl: String, xs: Double*): SRealVec[String] = data.vec(cl,IndexedSeq(xs:_*))

}
