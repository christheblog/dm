package org.cc.dm.algo.knn

import org.scalatest.FunSuite

import collection.immutable.IndexedSeq
import org.cc.dm.data._
import org.cc.dm.algo.knn.Knn._
import org.cc.dm.data
import org.cc.dm.datastructure.KHeap



class KnnTest extends FunSuite {

  val Euclidean = (v1: RealVec, v2: RealVec) => math.sqrt((v1 zip v2).map { case (x,y) => (x-y) * (x-y)}.sum)

  // Testing classic implementation

  test("Knn on an empty dataset should return an empty KHeap") {
    val ds = dataset()
    val target = vec(0.0,0.0,0.0,0.0)
    val res = knn(ds)(distfn=Euclidean)(k=3)(target)
    assert(KHeap.isEmpty(res))
  }

  test("Knn on a dataset of k elements should return an k-sized KHeap") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )
    val target = vec(0.0,0.0)
    val res = knn(ds)(distfn=Euclidean)(k=4)(target)
    assert(KHeap.size(res)==4)
  }

  test("Knn on a dataset of more than k elements should return an k-sized KHeap") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )
    val target = vec(0.0,0.0)
    val res = knn(ds)(distfn=Euclidean)(k=3)(target)
    assert(KHeap.size(res)==3)
  }

  test("Knn on a dataset of more than k elements should return an k-sized KHeap containing the k nearest neighbours of the target points") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )
    val target = vec(0.0,0.0)
    val res = knn(ds)(distfn=Euclidean)(k=2)(target)
    assert(KHeap.size(res)==2)
    implicit val order = Knn.NearestNeighbourDistanceOrdering
    val nn = KHeap.toStream(res)
    assert(nn.head._2==vec(2.0,2.0))      // furthest of the nn is first
    assert(nn.tail.head._2==vec(1.0,2.0)) // closest of the nn is last
  }

  test("Knn on a dataset with supervised vectors keeps classes") {
    val ds = dataset(
      svec("cl1",1.0,2.0),
      svec("cl2",2.0,2.0),
      svec("cl1",3.0,1.0),
      svec("cl2",4.0,3.0)
    )
    val target = vec(0.0,0.0)
    val res = knn(ds)(distfn=Euclidean)(k=2)(target)
    assert(KHeap.size(res)==2)
    implicit val order = Knn.NearestNeighbourDistanceOrdering
    val nn = KHeap.toStream(res)
    assert(nn.head._2==svec("cl2",2.0,2.0)) // furthest of the nn is first
    assert(nn.tail.head._2==svec("cl1",1.0,2.0)) // closest of the nn is last
  }



  // Helper

  def dataset(xs: RealVec*): RealDataset = IndexedSeq(xs:_*)
  def vec(xs: Double*): RealVec = IndexedSeq(xs:_*)
  def svec(cl: String, xs: Double*): SRealVec[String] = data.vec(cl,IndexedSeq(xs:_*))

}
